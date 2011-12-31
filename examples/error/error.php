<?php
include_once "../../parse_engine.php";
include_once "error.class";

$parser = new parse_engine(new parser());
//$parser->debug = true;

$tokens = array(
	array('ZZ', 'zz'),
	array('ZZ', 'zz'),
	array('YY', 'yy'),
    ';',
	array('ZZ', 'zz'),
	array('ZZ', 'zz'),
	';'
);

foreach($tokens as $token) {
    if (is_array($token)) {
        $parser->eat($token[0], $token[1]);
    } else {
        $parser->eat("'{$token}'", $token);
    }
}

$parser->eat_eof();

var_dump($parser->semantic);
var_dump($parser->errors);
