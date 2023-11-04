// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract BoundedWithdrawer {
    constructor () payable {}

    function withdraw(uint amount) public {
        require(amount <= 10 && amount <= address(this).balance);
        
        (bool succ,) = msg.sender.call{value: amount}("");
        require(succ);
    }
}
