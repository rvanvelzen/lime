#!/usr/bin/php -q
<?php
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

define('LIME_DIR', __DIR__);


function emit($str) {
	fputs(STDERR, $str . PHP_EOL);
}

class Bug extends Exception {
}

function bug($gripe = 'Bug found.') {
	throw new Bug($gripe);
}

function bug_if($fallacy, $gripe = 'Bug found.') {
	if ($fallacy) {
		throw new Bug($gripe);
	}
}

function bug_unless($assertion, $gripe = 'Bug found.') {
	if (!$assertion) {
		throw new Bug($gripe);
	}
}

require LIME_DIR . '/parse_engine.php';
require LIME_DIR . '/set.so.php';
require LIME_DIR . '/flex_token_stream.php';

function lime_token_reference($pos) {
	return '$tokens[' . $pos . ']';
}

function lime_token_reference_callback($foo) {
	if ($foo[1] === '$') {
		// always
		return '$result';
	} elseif (!ctype_digit($foo[1])) {
		return '$' . $foo[1];
	}

	return lime_token_reference($foo[1] - 1);
}

function lime_export($var) {
	if (is_array($var)) {
		$i = is_indexed($var);
		$out = array();
		foreach($var as $k => $v) {
			$out[] = (!$i ? lime_export($k).' => ' : '') . lime_export($v);
		}

		$result = 'array(' . PHP_EOL . preg_replace('~^~m', INDENT, implode(',' . PHP_EOL, $out)) . PHP_EOL . ')';
	} elseif (is_int($var) || is_float($var)) {
		$result = (string)$var;
	} elseif (is_string($var)) {
		$opt1 = '\'' . str_replace(array('\\', '\''), array('\\\\', '\\\''), $var) . '\'';
		$opt2 = $opt1;

		if (strpos($var, '$') === false) {
			$opt2 = '"' . str_replace(array('\\', '"'), array('\\\\', '\"'), $var) . '"';
		}

		if (strlen($opt1) <= strlen($opt2)) {
			$result = $opt1;
		} else {
			$result = $opt2;
		}
	} elseif (is_bool($var)) {
		$result = $var ? 'true' : 'false';
	} else {
		bug('Wrong type: ' . gettype($var));
	}

	return $result;
}

function is_indexed(array $array) {
	$i = 0;
	foreach($array as $k => $v) {
		if ($k !== $i++) {
			return false;
		}
	}

	return true;
}

function unindent($text) {
    if (preg_match('{\A[\r\n]*([ \t]+)[^\r\n]*+(?:[\r\n]++(?>\1[^\r\n]*+(?:[\r\n]+|\z)|[\r\n]+)+)?\z}', rtrim($text), $match)) {
        $text = preg_replace('{^' . $match[1] . '}m', '', $text);
    }

    return $text;
}

class cf_action {
	protected $code;

	public function __construct($code) {
		$this->code = $code;
	}
}

/**
 * Base class for parse table instructions. The main idea is to make the
 * subclasses responsible for conflict resolution among themselves. It also
 * forms a sort of interface to the parse table.
 */
abstract class step {
	public $sym;

	public function __construct(sym $sym) {
		$this->sym = $sym;
	}

	public function glyph() {
		return $this->sym->name;
	}

	public function sane() {
		return true;
	}

	abstract public function instruction();
	abstract public function decide($that);
}

class error extends step {
	public function sane() {
		return false;
	}

	public function instruction() {
		bug('This should not happen.');
	}

	public function decide($that) {
		// An error shall remain one
		return array($this);
	}
}

class shift extends step {
	public $q;
	public $rule;

	public function __construct(sym $sym, $q, rule $rule) {
		parent::__construct($sym);

		$this->q = $q;
		$this->rule = $rule;
	}

	public function instruction() {
		return 's ' . $this->q;
	}

	public function decide($that) {
		// shift-shift conflicts are impossible.
		// shift-accept conflicts are a bug.
		// so we can infer:
		bug_unless($that instanceof reduce);

		// That being said, the resolution is a matter of precedence.
		$shift_prec = $this->rule->right_prec;
		$reduce_prec = $that->rule->left_prec;

		// If we don't have defined precedence levels for both options,
		// then we default to shifting:
		if (!($shift_prec and $reduce_prec)) {
			return array($this, 1);
		}

		// Otherwise, use the step with higher precedence.
		if ($shift_prec > $reduce_prec) {
			return array($this, 0);
		}

		if ($reduce_prec > $shift_prec) {
			return array($that, 0);
		}

		// The "nonassoc" works by giving equal precedence to both options,
		// which means to put an error instruction in the parse table.
		return array(new error($this->sym), 0);
	}
}

class reduce extends step {
	public function __construct($sym, rule $rule) {
		parent::__construct($sym);
		$this->rule = $rule;
	}

	public function instruction() {
		return 'r ' . $this->rule->id;
	}

	function decide($that) {
		// This means that the input grammar has a reduce-reduce conflict.
		// Such things are considered an error in the input.
		throw new RRC($this, $that);

		// BISON would go with the first encountered reduce thus:
		// return $this;
	}
}

class accept extends step {
	public function __construct(sym $sym) {
		parent::__construct($sym);
	}

	public function instruction() {
		return 'a ' . $this->sym->name;
	}

	public function decide($that) {
		return array($this, 0);
	}
}

class RRC extends Exception {
	public function __construct($a, $b) {
		parent::__construct('Reduce-Reduce Conflict');

		$this->a = $a;
		$this->b = $b;
	}

	function make_noise() {
		emit(sprintf(
			'Reduce-Reduce Conflict:' . PHP_EOL . '%s' . PHP_EOL . '%s' . PHP_EOL . 'Lookahead is (%s)',
			$this->a->rule->text(),
			$this->b->rule->text(),
			$this->a->glyph()
		));
	}
}

class state {
	public $id;
	public $key;
	public $close;
	public $conflicts = 0;
	public $action = array();

	public function __construct($id, $key, $close) {
		$this->id = $id;
		$this->key = $key;
		$this->close = $close; // config key -> object
		ksort($this->close);
	}

	public function dump() {
		echo ' * ' . $this->id . ' / ' . $this->key . PHP_EOL;
		foreach ($this->close as $config) {
			$config->dump();
		}
	}

	public function add_shift(sym $sym, $state, $rule) {
		$this->add_instruction(new shift($sym, $state->id, $rule));
	}

	public function add_reduce(sym $sym, $rule) {
		$this->add_instruction(new reduce($sym, $rule));
	}

	public function add_accept(sym $sym) {
		$this->add_instruction(new accept($sym));
	}

	public function add_instruction(step $step) {
		$this->action[] = $step;
	}

	function find_reductions($lime) {
		// rightmost configurations followset yields reduce.
		foreach($this->close as $c) {
			if ($c->rightmost) {
				foreach ($c->follow->all() as $glyph) {
					$this->add_reduce($lime->sym($glyph), $c->rule);
				}
			}
		}
	}

	function resolve_conflicts() {
		// For each possible lookahead, find one (and only one) step to take.
		$table = array();
		foreach ($this->action as $step) {
			$glyph = $step->glyph();
			if (isset($table[$glyph])) {
				// There's a conflict. The shifts all came first, which
				// simplifies the coding for the step->decide() methods.
				try {
					list($table[$glyph], $conflict) = $table[$glyph]->decide($step);
					$this->conflicts += $conflict;
				} catch (RRC $e) {
					emit('State ' . $this->id . ':');
					$e->make_noise();
				}
			} else {
				// This glyph is yet unprocessed, so the step at hand is
				// our best current guess at what the grammar indicates.
				$table[$glyph] = $step;
			}
		}

		// Now that we have the correct steps chosen, this routine is oddly
		// also responsible for turning that table into the form that will
		// eventually be passed to the parse engine. (So FIXME?)
		$out = array();
		foreach ($table as $glyph => $step) {
			if ($step->sane()) {
				$out[$glyph] = $step->instruction();
			}
		}

		return $out;
	}

	function segment_config() {
		// Filter $this->close into categories based on the symbol_after_the_dot.
		$f = array();

		foreach ($this->close as $c) {
			if (($p = $c->symbol_after_the_dot)) {
				$f[$p->name][] = $c;
			}
		}

		return $f;
	}
}

class sym {
	public function __construct($name, $id) {
		$this->name = $name;
		$this->id = $id;
		$this->term = true;	// Until proven otherwise.
		$this->rule = array();
		$this->config = array();
		$this->lambda = false;
		$this->first = new set();
		$this->left_prec = $this->right_prec = 0;
	}

	public function summary() {
		$out = '';
		foreach ($this->rule as $rule) {
			$out .= $rule->text() . PHP_EOL;
		}

		return $out;
	}
}

class rule {
	public function __construct($id, $sym, $rhs, $code, $look, $replace, $prec_sym) {
		bug_unless(is_int($look));

		$this->id = $id;
		$this->sym = $sym;
		$this->rhs = $rhs;
		$this->code = $code;
		$this->look = $look;
		$this->replace = $replace;
		$this->prec_sym = $prec_sym;
		$this->left_prec = 0;
		$this->right_prec = 0;
		$this->first = array();
		$this->epsilon = count($rhs);
	}

	public function lhs_glyph() {
		return $this->sym->name;
	}

	public function determine_precedence() {
		// We may eventually expand to allow explicit prec_symbol declarations.
		// Until then, we'll go with the rightmost terminal, which is what
		// BISON does. People probably expect that. The leftmost terminal
		// is a reasonable alternative behaviour, but I don't see the big
		// deal just now.

		$prec_sym = $this->prec_sym;
		if (!$prec_sym) {
			$prec_sym = $this->rightmost_terminal();
		}

		if (!$prec_sym) {
			return;
		}

		$this->left_prec = $prec_sym->left_prec;
		$this->right_prec = $prec_sym->right_prec;
	}

	private function rightmost_terminal() {
		$symbol = null;
		$rhs = $this->rhs;

		while ($rhs) {
			$symbol = array_pop($rhs);
			if ($symbol->term) {
				break;
			}
		}

		return $symbol;
	}

	public function text() {
		$t = '(' . $this->id . ') ' . $this->lhs_glyph() . ' :=';

		foreach($this->rhs as $s) {
			$t .= '  ' . $s->name;
		}

		if (!$this->rhs) {
			$t .= '  ε';
		}

		return $t;
	}

	public function table(lime_language $lang) {
		return array(
			'symbol' => $this->lhs_glyph(),
			'len' => $this->look,
			'replace' => $this->replace,
			'code' => $lang->fixup($this->code),
			'text' => $this->text(),
		);
	}

	public function lambda() {
		foreach ($this->rhs as $sym) {
			if (!$sym->lambda) {
				return false;
			}
		}

		return true;
	}

	public function find_first() {
		$dot = count($this->rhs);
		$last = $this->first[$dot] = new set();
		while ($dot--) {
			$symbol_after_the_dot = $this->rhs[$dot];
			$first = $symbol_after_the_dot->first->all();

			bug_if(empty($first) and !$symbol_after_the_dot->lambda and $symbol_after_the_dot->name != 'error');

			$set = new set($first);
			if ($symbol_after_the_dot->lambda) {
				$set->union($last);
				if ($this->epsilon == $dot + 1) {
					$this->epsilon = $dot;
				}
			}

			$last = $this->first[$dot] = $set;
		}
	}

	public function teach_symbol_of_first_set() {
		$go = false;
		foreach ($this->rhs as $sym) {
			if ($this->sym->first->union($sym->first)) {
				$go = true;
			}

			if (!$sym->lambda) {
				break;
			}
		}

		return $go;
	}

	public function lambda_from($dot) {
		return $this->epsilon <= $dot;
	}

	public function leftmost($follow) {
		return new config($this, 0, $follow);
	}

	public function dotted_text($dot) {
		$out = $this->lhs_glyph() . ' :=';
		$idx = -1;
		foreach($this->rhs as $idx => $s) {
			if ($idx == $dot) {
				$out .= ' .';
			}

			$out .= '  ' . $s->name;
		}

		if (!$this->rhs) {
			$out .= '  ε';
		}

		if ($dot > $idx) {
			$out .= ' .';
		}

		return $out;
	}
}

class config {
	public function __construct($rule, $dot, $follow) {
		$this->rule = $rule;
		$this->dot = $dot;
		$this->key = $rule->id . '.' . $dot;
		$this->rightmost = count($rule->rhs) <= $dot;
		$this->symbol_after_the_dot = $this->rightmost ? null : $rule->rhs[$dot];
		$this->_blink = array();
		$this->follow = new set($follow);
		$this->_flink = array();

		bug_unless($this->rightmost or count($rule));
	}

	public function text() {
		return $this->rule->dotted_text($this->dot)
			. ' [ ' . implode(' ', $this->follow->all()) . ' ]';
	}

	public function blink($config) {
		$this->_blink[] = $config;
	}

	public function next() {
		bug_if($this->rightmost);

		$c = new config($this->rule, $this->dot+1, array());
		// Anything in the follow set for this config will also be in the next.
		// However, we link it backwards because we might wind up selecting a
		// pre-existing state, and the housekeeping is easier in the first half
		// of the program. We'll fix it before doing the propagation.
		$c->blink($this);

		return $c;
	}

	public function copy_links_from($that) {
		foreach($that->_blink as $c) {
			$this->blink($c);
		}
	}

	public function lambda() {
		return $this->rule->lambda_from($this->dot);
	}

	public function simple_follow() {
		return $this->rule->first[$this->dot + 1]->all();
	}

	public function epsilon_follows() {
		return $this->rule->lambda_from($this->dot + 1);
	}

	public function fixlinks() {
		foreach ($this->_blink as $that) {
			$that->_flink[] = $this;
		}

		$this->blink = array();
	}

	public function dump() {
		echo '   * ';
		echo $this->key . ' : ';
		echo $this->rule->dotted_text($this->dot);
		echo $this->follow->text();
		foreach ($this->_flink as $c) {
			echo $c->key . ' / ';
		}

		echo PHP_EOL;
	}
}

class lime {
	public $parser_class = 'parser';

	public $descr = array();

	public function __construct() {
		$this->p_next = 1;
		$this->sym = array();
		$this->rule = array();
		$this->start_symbol_set = array();
		$this->state = array();
		$this->stop = $this->sym('#');
		$this->expect_conflicts = null;
		$this->conflicts = 0;

		if ($err = $this->sym('error')) {
			$err->term = false;
		}

		$this->lang = new lime_language_php();
	}

	function language() {
		return $this->lang;
	}

	function build_parser() {
		$this->add_start_rule();

		foreach ($this->rule as $r) {
			$r->determine_precedence();
		}

		$this->find_sym_lamdba();
		$this->find_sym_first();

		foreach ($this->rule as $rule) {
			$rule->find_first();
		}

		$initial = $this->find_states();
		$this->fixlinks();
		// $this->dump_configurations();
		$this->find_follow_sets();

		foreach($this->state as $s) {
			$s->find_reductions($this);
		}

		$i = $this->resolve_conflicts();
		$a = $this->rule_table();
		$qi = $initial->id;
		$d = $this->descr;

		if ($this->expect_conflicts !== null && $this->expect_conflicts != $this->conflicts) {
			throw new Bug($this->expect_conflicts .' conflicts expected, got ' . $this->conflicts);
		}

		return $this->lang->ptab_to_class($this->parser_class, compact('a', 'qi', 'i', 'd'));
	}

	function rule_table() {
		$s = array();

		foreach ($this->rule as $i => $r) {
			$s[$i] = $r->table($this->lang);
		}

		return $s;
	}

	function add_rule($symbol, $rhs, $code) {
		$this->add_raw_rule($symbol, $rhs, $code, count($rhs), true);
	}

	function trump_up_bogus_lhs($real) {
		return "'{$real}'" . count($this->rule);
	}

	function add_raw_rule($lhs, $rhs, $code, $look, $replace, $prec_sym) {
		$sym = $this->sym($lhs);
		$sym->term = false;

		if (!$rhs) {
			$sym->lambda = true;
		}

		$rs = array();

		foreach ($rhs as $str) {
			$rs[] = $this->sym($str);
		}

		$rid = count($this->rule);
		$r = new rule($rid, $sym, $rs, $code, $look, $replace, $prec_sym);
		$this->rule[$rid] = $r;
		$sym->rule[] = $r;
	}

	function sym($str, $description = null) {
		if (!isset($this->sym[$str])) {
			$this->sym[$str] = new sym($str, count($this->sym));
		}

		if ($description) {
			$this->descr[$str] = $description;
		}

		return $this->sym[$str];
	}

	function summary() {
		$out = '';

		foreach ($this->sym as $sym) {
			if (!$sym->term) {
				$out .= $sym->summary();
			}
		}

		return $out;
	}

	private function find_sym_lamdba() {
		do {
			$go = false;
			foreach ($this->sym as $sym) {
				if (!$sym->lambda) {
					foreach ($sym->rule as $rule) {
						if ($rule->lambda()) {
							$go = true;
							$sym->lambda = true;
						}
					}
				}
			}
		} while ($go);
	}

	private function teach_terminals_first_set() {
		foreach ($this->sym as $sym) {
			if ($sym->term) {
				$sym->first->add($sym->name);
			}
		}
	}

	private function find_sym_first() {
		$this->teach_terminals_first_set();

		do {
			$go = false;
			foreach ($this->rule as $r) {
				if ($r->teach_symbol_of_first_set()) {
					$go = true;
				}
			}
		} while ($go);
	}

	function add_start_rule() {
		$rewrite = new lime_rewrite("'start'");
		$rhs = new lime_rhs();
		$rhs->add(new lime_glyph($this->deduce_start_symbol()->name, null));
		$rewrite->add_rhs($rhs);
		$rewrite->update($this);
	}

	protected function deduce_start_symbol() {
		$candidate = current($this->start_symbol_set);

		// Did the person try to set a start symbol at all?
		if (!is_object($candidate)) {
			return $this->first_rule_lhs();
		}

		// Do we actually have such a symbol on the left of a rule?
		if ($candidate->terminal) {
			return $this->first_rule_lhs();
		}

		// Ok, it's a decent choice. We need to return the symbol entry.
		return $this->sym($candidate);
	}

	private function first_rule_lhs() {
		reset($this->rule);
		$r = current($this->rule);
		return $r->sym;
	}

	/**
	 * Build an initial state. This is a recursive process which digs out
	 * the LR(0) state graph.
	 */
	function find_states() {
		$start_glyph = "'start'";
		$sym = $this->sym($start_glyph);
		$basis = array();

		foreach($sym->rule as $rule) {
			$c = $rule->leftmost(array('#'));
			$basis[$c->key] = $c;
		}

		$initial = $this->get_state($basis);
		$initial->add_accept($sym);

		return $initial;
	}

	function get_state($basis) {
		$key = array_keys($basis);
		sort($key);
		$key = implode(' ', $key);

		if (isset($this->state[$key])) {
			// Copy all the links around...
			$state = $this->state[$key];

			foreach($basis as $config) {
				$state->close[$config->key]->copy_links_from($config);
			}

			return $state;
		} else {
			$close = $this->state_closure($basis);
			$this->state[$key] = $state = new state(count($this->state), $key, $close);
			$this->build_shifts($state);

			return $state;
		}
	}

	private function state_closure($q) {
		// $q is a list of config.
		$close = array();
		while ($config = array_pop($q)) {
			if (isset($close[$config->key])) {
				$close[$config->key]->copy_links_from($config);
				$close[$config->key]->follow->union($config->follow);
				continue;
			}

			$close[$config->key] = $config;

			$symbol_after_the_dot = $config->symbol_after_the_dot;
			if (!$symbol_after_the_dot) {
				continue;
			}

			if (!$symbol_after_the_dot->term) {
				foreach ($symbol_after_the_dot->rule as $r) {
					$station = $r->leftmost($config->simple_follow());

					if ($config->epsilon_follows()) {
						$station->blink($config);
					}

					$q[] = $station;
				}
			}
		}

		return $close;
	}

	function build_shifts($state) {
		foreach ($state->segment_config() as $glyph => $segment) {
			$basis = array();
			foreach ($segment as $preshift) {
				$postshift = $preshift->next();
				$basis[$postshift->key] = $postshift;
			}

			$dest = $this->get_state($basis);
			$state->add_shift($this->sym($glyph), $dest, $segment[0]->rule);
		}
	}

	function fixlinks() {
		foreach ($this->state as $s) {
			foreach ($s->close as $c) {
				$c->fixlinks();
			}
		}
	}

	function find_follow_sets() {
		$q = array();

		foreach ($this->state as $s) {
			foreach ($s->close as $c) {
				$q[] = $c;
			}
		}

		while ($q) {
			$c = array_shift($q);

			foreach ($c->_flink as $d) {
				if ($d->follow->union($c->follow)) {
					$q[] = $d;
				}
			}
		}
	}

	private function set_assoc($ss, $l, $r) {
		$p = ($this->p_next++) * 2;
		foreach ($ss as $glyph) {
			$s = $this->sym($glyph);

			$s->left_prec = $p + $l;
			$s->right_prec = $p + $r;
		}
	}

	function left_assoc($ss) {
		$this->set_assoc($ss, 1, 0);
	}

	function right_assoc($ss) {
		$this->set_assoc($ss, 0, 1);
	}

	function non_assoc($ss) {
		$this->set_assoc($ss, 0, 0);
	}

	private function resolve_conflicts() {
		// For each state, try to find one and only one
		// thing to do for any given lookahead.
		$i = array();

		foreach ($this->state as $s) {
			$i[$s->id] = $s->resolve_conflicts();
			$this->conflicts += $s->conflicts;
		}

		return $i;
	}

	function dump_configurations() {
		foreach ($this->state as $q) {
			$q->dump();
		}
	}

	function dump_first_sets() {
		foreach ($this->sym as $s) {
			echo ' * ';
			echo $s->name . ' : ';
			echo $s->first->text();
			echo PHP_EOL;
		}
	}

	function add_rule_with_actions($lhs, $rhs) {
		// First, make sure this thing is well-formed.
		if(!is_object(end($rhs))) {
			$rhs[] = new cf_action('');
		}

		// Now, split it into chunks based on the actions.
		$look = -1;
		$subrule = array();
		$subsymbol = '';

		while ($rhs) {
			$it = array_shift($rhs);
			++$look;

			if (is_string($it)) {
				$subrule[] = $it;
			} else {
				$code = $it->code;
				// It's an action.
				// Is it the last one?
				if ($rhs) {
					// no.
					$subsymbol = $this->trump_up_bogus_lhs($lhs);
					$this->add_raw_rule($subsymbol, $subrule, $code, $look, false);
					$subrule = array($subsymbol);
				} else {
					// yes.
					$this->add_raw_rule($lhs, $subrule, $code, $look, true);
				}
			}
		}
	}

	function pragma($type, $args) {
		switch ($type) {
		case 'left':
			$this->left_assoc($args);
			break;
		case 'right':
			$this->right_assoc($args);
			break;
		case 'nonassoc':
			$this->non_assoc($args);
			break;
		case 'start':
			$this->start_symbol_set = $args;
			break;
		case 'class':
			$this->parser_class = $args[0];
			break;
		case 'expect':
			if (!ctype_digit($args[0])) {
				emit(sprintf('Bad expect pragma: %s', $args[0]));
				exit(1);
			}

			$this->expect_conflicts = $args[0];
			break;
		case 'token':
			$this->sym($args[0], @$args[1]);
			break;
		default:
			emit(sprintf('Bad Parser Pragma: (%s)', $type));
			exit(1);
		}
	}
}

class lime_language {
}

class lime_language_php extends lime_language {
	protected function result_code($expr) {
		return '$result = ' . $expr . ';' . PHP_EOL;
	}

	public function default_result() {
		return $this->result_code('reset($tokens)');
	}

	public function result_pos($pos) {
		return $this->result_code(lime_token_reference($pos));
	}

	public function bind($name, $pos) {
		return '$' . $name . ' = &$tokens[' . $pos . '];' . PHP_EOL;
	}

	public function fixup($code) {
		return preg_replace_callback('~\$(\d+|\$)~', function ($foo) {
			if ($foo[1] === '$') {
				// always
				return '$result';
			}

			return lime_token_reference($foo[1] - 1);
		}, $code);
	}

	function to_php($code) {
		return $code;
	}

	public function ptab_to_class($parser_class, $ptab) {
		$code  = '';
		$code .= 'public $qi = ' . lime_export($ptab['qi'], true) . ';' . PHP_EOL;
		$code .= 'public $i = ' . lime_export($ptab['i'], true) . ';' . PHP_EOL;
		$code .= 'public $d = ' . lime_export($ptab['d'], true) . ';' . PHP_EOL;
		$code .= 'public $errors = array();' . PHP_EOL;

		$rc = array();
		$method = array();
		$rules = array();

		foreach($ptab['a'] as $k => $a) {
			$symbol = preg_replace('/[^\w]/', '', $a['symbol']);
			$rn = @++$rc[$symbol];
			$mn = 'reduce_' . $k . '_' . $symbol . '_' . $rn;
			$method[$k] = $mn;
			$comment = '// ' . $a['text'] . PHP_EOL;
			$php = $this->to_php($a['code']);

			$code .= 'function ' . $mn . '(' . LIME_CALL_PROTOCOL . ') {' . PHP_EOL .
				rtrim(preg_replace('~^~m', INDENT, $comment . $php)) . PHP_EOL .
			'}' .
			PHP_EOL .
			PHP_EOL;

			unset($a['code']);
			unset($a['text']);
			$rules[$k] = $a;
		}

		$code .= 'public $method = ' . lime_export($method, true) . ';' . PHP_EOL;
		$code .= 'public $a = '.lime_export($rules, true) . ';' . PHP_EOL;

		return 'class ' . $parser_class . ' extends lime_parser {' . PHP_EOL .
			preg_replace(array('~^~m', '~^\h+$~m'), array(INDENT, ''), $code) .
		'}' . PHP_EOL;
	}
}

class lime_rhs {
	public $prec_glyph;

	function __construct() {
		// Construct and add glyphs and actions in whatever order.
		// Then, add this to a lime_rewrite.
		//
		// Don't call install_rule.
		// The rewrite will do that for you when you "update" with it.
		$this->rhs = array();
	}

	public function add(lime_slot $slot) {
		$this->rhs[] = $slot;
	}

	public function set_prec_glyph($glyph) {
		$this->prec_glyph = $glyph;
	}

	function install_rule(lime $lime, $lhs) {
		// This is the part that has to break the rule into subrules if necessary.
		$rhs = $this->rhs;
		// First, make sure this thing is well-formed.
		if (!(end($rhs) instanceof lime_action)) {
			$rhs[] = new lime_action('', null);
		}

		$prec_sym = null;
		if ($this->prec_glyph) {
			$prec_sym = $lime->sym($this->prec_glyph);
		}

		// Now, split it into chunks based on the actions.

		$lang = $lime->language();
		$result_code = $lang->default_result();
		$look = -1;
		$subrule = array();
		$subsymbol = '';
		$preamble = '';

		while ($rhs) {
			$it = array_shift($rhs);
			++$look;

			if ($it instanceof lime_glyph) {
				$subrule[] = $it->data;
			} elseif ($it instanceof lime_action) {
				$code = unindent($it->data);
				// It's an action.
				// Is it the last one?
				if ($rhs) {
					// no.
					$subsymbol = $lime->trump_up_bogus_lhs($lhs);
					$action = $lang->default_result() . $preamble . $code;
					$lime->add_raw_rule($subsymbol, $subrule, $action, $look, false, $prec_sym);
					$subrule = array($subsymbol);
				} else {
					// yes.
					$action = $result_code . $preamble . $code;
					$lime->add_raw_rule($lhs, $subrule, $action, $look, true, $prec_sym);
				}
			} else {
				impossible();
			}

			if ($it->name == '$') {
				$result_code = $lang->result_pos($look);
			} elseif ($it->name) {
				$preamble .= $lang->bind($it->name, $look);
			}
		}
	}
}

class lime_rewrite {
	function __construct($glyph) {
		// Construct one of these with the name of the lhs.
		// Add some rhs-es to it.
		// Finally, "update" the lime you're building.
		$this->glyph = $glyph;
		$this->rhs = array();
	}

	function add_rhs(lime_rhs $rhs) {
		$this->rhs[] = $rhs;
	}

	function update(lime $lime) {
		foreach ($this->rhs as $rhs) {
			$rhs->install_rule($lime, $this->glyph);
		}
	}
}

/**
 * This keeps track of one position in an rhs.
 *  We specialize to handle actions and glyphs.
 *
 * If there is a name for the slot, we store it here.
 * Later on, this structure will be consulted in the formation of
 * actual production rules.
 */
class lime_slot {
	public function __construct($data, $name) {
		$this->data = $data;
		$this->name = $name;
	}

	public function preamble($pos) {
		if (strlen($this->name) > 0) {
			return '$' . $this->name . ' = &$tokens[' . $pos . '];' . PHP_EOL;
		}
	}
}

class lime_glyph extends lime_slot {
}
class lime_action extends lime_slot {
}


/**
 * This function isn't too terribly interesting to the casual observer.
 * You're probably better off looking at parse_lime_grammar() instead.
 *
 * Ok, if you insist, I'll explain.
 *
 * The input to Lime is a CFG parser definition. That definition is
 * written in some language. (The Lime language, to be exact.)
 * Anyway, I have to parse the Lime language and compile it into a
 * very complex data structure from which a parser is eventually
 * built. What better way than to use Lime itself to parse its own
 * language? Well, it's almost that simple, but not quite.

 * The Lime language is fairly potent, but a restricted subset of
 * its features was used to write a metagrammar. Then, I hand-translated
 * that metagrammar into another form which is easy to snarf up.
 * In the process of reading that simplified form, this function
 * builds the same sort of data structure that later gets turned into
 * a parser. The last step is to run the parser generation algorithm,
 * eval() the resulting PHP code, and voila! With no hard work, I can
 * suddenly read and comprehend the full range of the Lime language
 * without ever having written an algorithm to do so. It feels like magic.
 */
function lime_bootstrap() {
	$bootstrap = LIME_DIR . '/lime.bootstrap';
	$lime = new lime();
	$lime->parser_class = 'lime_metaparser';
	$rhs = array();

	bug_unless(is_readable($bootstrap));

	foreach(file($bootstrap) as $l) {
		$a = preg_split('~(?<=\s|^):(?=\s|$)~', $l, 2);

		if (count($a) == 2) {
			list($pattern, $code) = $a;
			$sl = new lime_rhs();
			$pattern = trim($pattern);

			if (strlen($pattern) > 0) {
				foreach (explode(' ', $pattern) as $glyph) {
					$sl->add(new lime_glyph($glyph, null));
				}
			}

			$sl->add(new lime_action($code, NULL));
			$rhs[] = $sl;
		} else {
			if (preg_match('~^to (\w+)$~', $l, $r)) {
				$g = $r[1];
				$rw = new lime_rewrite($g);

				foreach($rhs as $b) {
					$rw->add_rhs($b);
				}

				$rw->update($lime);
				$rhs = array();
			}
		}
	}

	$parser_code = $lime->build_parser();
	eval($parser_code);
}

/**
 * The voodoo is in the way I do lexical processing on grammar definition
 * files. They contain embedded bits of PHP, and it's important to keep
 * track of things like strings, comments, and matched braces. It seemed
 * like an ideal problem to solve with GNU flex, so I wrote a little
 * scanner in flex and C to dig out the tokens for me. Of course, I need
 * the tokens in PHP, so I designed a simple binary wrapper for them which
 * also contains line-number information, guaranteed to help out if you
 * write a grammar which surprises the parser in any manner.
 */
class voodoo_scanner extends flex_scanner {
	function executable() { return LIME_DIR.'/lime_scan_tokens'; }
}

/**
 * This is a good function to read because it teaches you how to interface
 * with a Lime parser. I've tried to isolate out the bits that aren't
 * instructive in that regard.
 */
function parse_lime_grammar($path) {
	if (!class_exists('lime_metaparser', false)) {
		lime_bootstrap();
	}

	$parse_engine = new parse_engine(new lime_metaparser());
	$scanner = new voodoo_scanner($path);

	try {
		// The result of parsing a Lime grammar is a Lime object.
		$lime = $scanner->feed($parse_engine);
		// Calling its build_parser() method gets the output PHP code.
		return $lime->build_parser();
	} catch (parse_error $e) {
		die ($e->getMessage() . " in {$path} line {$scanner->lineno}." . PHP_EOL);
	}
}

if ($_SERVER['argv']) {
	$code = '';

	if (!defined('INDENT')) {
		foreach($_SERVER['argv'] as $i => $opt) {
			if ($opt === '--indent') {
				$val = @$_SERVER['argv'][$i + 1];
				if (!$val) {
					define('INDENT', '  ');
				} else {
					define('INDENT', $val == 1 ? "\t" : str_repeat(' ', $val));
				}
				unset($_SERVER['argv'][$i], $_SERVER['argv'][$i + 1]);
				break;
			}
		}

		if (!defined('INDENT')) {
			define('INDENT', '  ');
		}
	}

	array_shift($_SERVER['argv']); // Strip out the program name.

	$timer = microtime(true);

	foreach ($_SERVER['argv'] as $path) {
		$code .= parse_lime_grammar($path);
	}

	echo <<<CODE
<?php
/*
 *** DON'T EDIT THIS FILE! ***
 *
 * This file was automatically generated by the Lime parser generator.
 * The real source code you should be looking at is in one or more
 * grammar files in the Lime format.
 *
 * THE ONLY REASON TO LOOK AT THIS FILE is to see where in the grammar
 * file that your error happened, because there are enough comments to
 * help you debug your grammar.

 * If you ignore this warning, you're shooting yourself in the brain,
 * not the foot.
 */
{$code}
CODE;

echo PHP_EOL
	. '// Time: ' . (microtime(true) - $timer) . ' seconds' . PHP_EOL
	. '// Memory: ' . memory_get_peak_usage() . ' bytes' . PHP_EOL;
}
