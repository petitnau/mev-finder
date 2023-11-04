// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Crowdfund {
    address receiver;
    uint goal;
    uint start;
    mapping(address => uint) amount;

    constructor(uint _goal) {
        receiver = msg.sender;
        goal = _goal;
        start = block.number;
    }

    function donate() public payable {
        require(block.number < start + 10);
        require(msg.value > 0);
        amount[msg.sender] += msg.value;
    }

    function claim() public {
        require(block.number >= start + 10);
        require(address(this).balance >= goal);

        (bool succ,) = receiver.call{value: address(this).balance}("");
        require(succ);
    }

    function refund() public {
        require(block.number >= start + 10);
        require(address(this).balance < goal);
        require(amount[msg.sender] > 0);

        (bool succ,) = msg.sender.call{value: amount[msg.sender]}("");
        require(succ);
    }
}
