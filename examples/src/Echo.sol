// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Echo {
    function echo() public payable {
        (bool succ,) = msg.sender.call{value: msg.value}("");
        require(succ);
    }
}
