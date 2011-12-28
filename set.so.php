<?php

/*
File: set.so.php
License: GPL
Purpose: We should really have a "set" data type. It's too useful.
*/

class set {
	public function __construct(array $list = array()) {
		$this->data = array_count_values($list);
	}

	public function has($item) {
		return isset($this->data[$item]);
	}

	public function add($item) {
		$this->data[$item] = true;
	}

	public function del($item) {
		unset($this->data[$item]);
		return $item;
	}

	public function all() {
		return array_keys($this->data);
	}

	public function one() {
		return key($this->data);
	}

	public function count() {
		return count($this->data);
	}

	public function pop() {
		return $this->del($this->one());
	}

	public function union($that) {
		$progress = false;
		foreach ($that->all() as $item) {
			if (!$this->has($item)) {
				$this->add($item);
				$progress = true;
			}
		}

		return $progress;
	}

	public function text() {
		return ' { ' . implode(' ', $this->all()) . ' } ';
	}
}
