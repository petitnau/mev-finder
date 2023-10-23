// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Allower {
    uint allow;
    address owner;

    function setAllow() public {
        require(msg.sender == 0x0000000000000000000000000000000000000002);
        allow = 1;
    }

    function withdraw(uint amount) public payable {
        require(allow == 1);
        (bool succ,) = msg.sender.call{value: amount}("");
        require(succ);
    }
}
