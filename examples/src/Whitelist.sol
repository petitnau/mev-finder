// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Whitelist {
    address receiver;

    function pay() public {
        require(msg.sender == 0x230A1AC45690B9Ae1176389434610B9526d2f21b);
        (bool succ,) = msg.sender.call{value: address(this).balance}("");
        require(succ);
    }
}
