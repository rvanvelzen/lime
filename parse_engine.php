<?php
/**
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

define('LIME_CALL_PROTOCOL', '$tokens, &$result');

abstract class lime_parser {
}

/**
 * The input doesn't match the grammar
 */
class parse_error extends Exception {
}

/**
 * Bug, I made a mistake
 */
class parse_bug extends Exception {
}

class parse_unexpected_token extends parse_error {
	public function __construct($type, $state) {
		parent::__construct("Unexpected token of type {$type}");

		$this->type = $type;
		$this->state = $state;
	}
}

class parse_premature_eof extends parse_error {
	public function __construct(array $expect) {
		parent::__construct('Premature EOF');
	}
}

class parse_stack {
	public $q;
	public $qs = array();
	/**
	 * Stack of semantic actions
	 */
	public $ss = array();

	public function __construct($qi) {
		$this->q = $qi;
	}

	public function shift($q, $semantic) {
		$this->ss[] = $semantic;
		$this->qs[] = $this->q;

		$this->q = $q;

		// echo "Shift $q -- $semantic\n";
	}

	public function top_n($n) {
		if (!$n) {
			return array();
		}

		return array_slice($this->ss, 0 - $n);
	}

	public function pop_n($n) {
		if (!$n) {
			return array();
		}

		$qq = array_splice($this->qs, 0 - $n);
		$this->q = $qq[0];

		return array_splice($this->ss, 0 - $n);
	}

	public function occupied() {
		return !empty($this->ss);
	}

	public function index($n) {
		if ($n) {
			$this->q = $this->qs[count($this->qs) - $n];
		}
	}

	public function text() {
		return $this->q . ' : ' . implode(' . ', array_reverse($this->qs));
	}
}

class parse_engine {
	public $debug = false;

	public $parser;
	public $qi;
	public $rule;
	public $step;
	public $descr;
	/**
	 * @var boolean
	 */
	public $accept;
	/**
	 * @var parse_stack
	 */
	public $stack;

	public function __construct($parser) {
		$this->parser = $parser;
		$this->qi = $parser->qi;
		$this->rule = $parser->a;
		$this->step = $parser->i;
		$this->descr = $parser->d;

		$this->reset();
	}

	public function reset() {
		$this->accept = false;
		$this->stack = new parse_stack($this->qi);
		$this->parser->errors = array();
	}

	private function enter_error_tolerant_state() {
		while ($this->stack->occupied()) {
			if ($this->has_step_for('error')) {
				return true;
			}

			if ($this->debug) echo "Dropped an item from the stack, {" . implode(', ', $this->get_steps()) . "} left\n";
			if ($this->debug) echo 'Currently in state ' . $this->state() . "\n";

			$this->drop();
		}

		return false;
	}

	private function drop() {
		$this->stack->pop_n(1);
	}

	/*
	 * So that I don't get any brilliant misguided ideas:
	 *
	 * The "accept" step happens when we try to eat a start symbol.
	 * That happens because the reductions up the stack at the end
	 * finally (and symetrically) tell the parser to eat a symbol
	 * representing what they've just shifted off the end of the stack
	 * and reduced. However, that doesn't put the parser into any
	 * special different state. Therefore, it's back at the start
	 * state.
	 *
	 * That being said, the parser is ready to reduce an EOF to the
	 * empty program, if given a grammar that allows them.
	 *
	 * So anyway, if you literally tell the parser to eat an EOF
	 * symbol, then after it's done reducing and accepting the prior
	 * program, it's going to think it has another symbol to deal with.
	 * That is the EOF symbol, which means to reduce the empty program,
	 * accept it, and then continue trying to eat the terminal EOF.
	 *
	 * This infinte loop quickly runs out of memory.
	 *
	 * That's why the real EOF algorithm doesn't try to pretend that
	 * EOF is a terminal. Like the invented start symbol, it's special.
	 *
	 * Instead, we pretend to want to eat EOF, but never actually
	 * try to get it into the parse stack. (It won't fit.) In short,
	 * we look up what reduction is indicated at each step in the
	 * process of rolling up the parse stack.
	 *
	 * The repetition is because one reduction is not guaranteed to
	 * cascade into another and clean up the entire parse stack.
	 * Rather, it will instead shift each partial production as it
	 * is forced to completion by the EOF lookahead.
	 */
	public function eat_eof() {
		// We must reduce as if having read the EOF symbol
		do {
			// and we have to try at least once, because if nothing
			// has ever been shifted, then the stack will be empty
			// at the start.
			list($opcode, $operand) = $this->step_for('#');

			switch ($opcode) {
			case 'r':
				$this->reduce($operand);
				break;
			case 'e':
				$this->premature_eof();
				break;
			default:
				throw new parse_bug();
			}
		} while ($this->stack->occupied());

		// If the sentence is well-formed according to the grammar, then
		// this will eventually result in eating a start symbol, which
		// causes the "accept" instruction to fire. Otherwise, the
		// step('#') method will indicate an error in the syntax, which
		// here means a premature EOF.
		//
		// Incidentally, some tremendous amount of voodoo with the parse
		// stack might help find the beginning of some unfinished
		// production that the sentence was cut off during, but as a
		// general rule that would require deeper knowledge.
		if (!$this->accept) {
			throw new parse_bug();
		}

		return $this->semantic;
	}

	private function premature_eof() {
		$seen = array();

		$expect = $this->get_steps();

		while ($this->enter_error_tolerant_state() || $this->has_step_for('error')) {
			if (isset($seen[$this->state()])) {
				// This means that it's pointless to try here.
				// We're guaranteed that the stack is occupied.
				$this->drop();
				continue;
			}

			$seen[$this->state()] = true;

			$this->eat('error', 'Premature EOF');

			if ($this->has_step_for('#')) {
				// Good. We can continue as normal.
				return;
			} else {
				// That attempt to resolve the error condition
				// did not work. There's no point trying to
				// figure out how much to slice off the stack.
				// The rest of the algorithm will make it happen.
			}
		}

		throw new parse_premature_eof($expect);
	}

	private function current_row() {
		return $this->step[$this->state()];
	}

	private function step_for($type) {
		$row = $this->current_row();
		if (!isset($row[$type])) {
			return array('e', $this->stack->q);
		}

		return explode(' ', $row[$type]);
	}

	private function get_steps() {
		$out = array();
		foreach($this->current_row() as $type => $row) {
			foreach($this->rule as $rule) {
				if ($rule['symbol'] == $type) {
					continue 2;
				}
			}

			list($opcode) = explode(' ', $row, 2);
			if ($opcode != 'e') {
				$out[] = $type;
			}
		}

		return $out;
	}

	private function has_step_for($type) {
		$row = $this->current_row();
		return isset($row[$type]);
	}

	private function state() {
		return $this->stack->q;
	}

	function eat($type, $semantic) {
		// assert('$type == trim($type)');
		if ($this->debug) echo "Trying to eat a ($type)\n";
		list($opcode, $operand) = $this->step_for($type);

		switch ($opcode) {
		case 's':
			if ($this->debug) echo "shift $type to state $operand\n";
			$this->stack->shift($operand, $semantic);
			// echo $this->stack->text()." shift $type<br/>\n";
			break;
		case 'r':
			if ($this->debug) echo "Reducing $type via rule $operand\n";
			$this->reduce($operand);
			$this->eat($type, $semantic);
			// Yes, this is tail-recursive. It's also the simplest way.
			break;
		case 'a':
			if ($this->stack->occupied()) {
				throw new parse_bug('Accept should happen with empty stack.');
			}

			$this->accept = true;
			if ($this->debug) echo ("Accept\n\n");
			$this->semantic = $semantic;
			break;
		case 'e':
			// This is thought to be the uncommon, exceptional path, so
			// it's OK that this algorithm will cause the stack to
			// flutter while the parse engine waits for an edible token.
			if ($this->debug) echo "($type) causes a problem.\n";

			// get these before doing anything
			$expected = $this->get_steps();

			$this->parser->errors[] = $this->descr($type, $semantic) . ' not expected, expected {' . implode(', ', $expected) . '}';

			if ($this->debug) echo "Possibilities before error fixing: {" . implode(', ', $expected) . "}\n";

			if ($this->enter_error_tolerant_state() || $this->has_step_for('error')) {
				$this->eat('error', end($this->errors));
				if ($this->has_step_for($type)) {
					$this->eat($type, $semantic);
				}
			} else {
				// If that didn't work, give up:
				throw new parse_error('Parse Error: ' . $this->descr($type, $semantic) . ' not expected, expected {' . implode(', ', $expected) . '}');
			}
			break;
		default:
			throw new parse_bug("Bad parse table instruction " . htmlspecialchars($opcode));
		}
	}

	private function descr($type, $semantic) {
		if (isset($this->descr[$type])) {
			return $this->descr[$type];
		} else {
			return $type . ' (' . $semantic . ')';
		}
	}

	private function reduce($rule_id) {
		$rule = $this->rule[$rule_id];
		$len = $rule['len'];
		$semantic = $this->perform_action($rule_id, $this->stack->top_n($len));

		//echo $semantic.br();
		if ($rule['replace']) {
			$this->stack->pop_n($len);
		} else {
			$this->stack->index($len);
		}

		$this->eat($rule['symbol'], $semantic);
	}

	private function perform_action($rule_id, $slice) {
		// we have this weird calling convention....
		$result = null;
		$method = $this->parser->method[$rule_id];

		//if ($this->debug) echo "rule $id: $method\n";
		$this->parser->$method($slice, $result);

		return $result;
	}
}
