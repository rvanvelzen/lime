This program is like a calculator. Type in lines of math, and it will
print the results. You can set a variable with:
	foo = 12 + 7.3
and use it in another calculation like:
	23.14 - foo

<?

include_once "../parse_engine.php";
include_once "calc.class";



function tokenize($line) {
	// Numbers are tokens, as are all other non-whitespace characters.
	// Note: This isn't a particularly efficent tokenizer, but it gets the
	// job done.
	$out = array();
	while (strlen($line)) {
		$line = trim($line);
		if (preg_match('/^[0-9]+(\.[0-9]*)?/', $line, $regs)) {
			# It's a number
			$out[] = $regs[0];
			$line = substr($line, strlen($regs[0]));
		} else if (preg_match('/^[A-Za-z]+/', $line, $regs)) {
			# It's a variable name
			$out[] = $regs[0];
			$line = substr($line, strlen($regs[0]));
		} else {
			# It's some other character
			$out[] = $line[0];
			$line = substr($line, 1);
		}
	}
	return $out;
}

$symbol_table = array();
function set_variable($v, $e) {
	global $symbol_table;
	$symbol_table[$v] = $e;
}
function get_variable($v) {
	global $symbol_table;
	return doubleval($symbol_table[$v]);
}

function calculate($line) {
	global $parser;
	if (!strlen($line)) return;
	try {
		$parser->reset();
		foreach(tokenize($line) as $t) {
			if (is_numeric($t)) $parser->eat('num', doubleval($t));
			else if (ctype_alpha($t)) $parser->eat('var', $t);
			else $parser->eat("'$t'", null);
		}
		$parser->eat_eof();
	} catch (parse_error $e) {
		echo $e->getMessage(), "\n";
	}
}

$parser = new parse_engine(new calc());
while ($line = fgets(STDIN)) calculate(trim($line));



