// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Whitelist {
    address receiver;

    function pay() public {
        require(msg.sender == 0x0000000000000000000000000000000000000001);
        (bool succ,) = msg.sender.call{value: address(this).balance}("");
        require(succ);
    }
}
