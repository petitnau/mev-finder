// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Summer {
    function get(uint amount) public pure returns (uint) {
        return amount + 5;
    }
}
