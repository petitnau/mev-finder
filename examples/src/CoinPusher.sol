// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract CoinPusher {
    constructor () payable {}

    function play() public payable {
        require(msg.value > 0);
        if (address(this).balance >= 100) {
            (bool succ,) = msg.sender.call{value: address(this).balance}("");
            require(succ);
        }
    }
}
