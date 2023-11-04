// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract AirDrop {
    constructor () payable {}

    function init() public payable {
        require(address(this).balance == 0);
    }

    function withdraw() public {
        (bool succ,) = msg.sender.call{value: address(this).balance}("");
        require(succ);
    }
}
