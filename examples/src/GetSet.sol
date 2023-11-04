// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract GetSet {
    uint value;

    function get() public view returns (uint) {
        return value; 
    }

    function set(uint _value) public {
        value = _value;
    }
}
