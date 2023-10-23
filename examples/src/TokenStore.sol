// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

import "./IERC20.sol";

contract TokenStore {
    IERC20 constant t0 = IERC20(0x230A1AC45690B9Ae1176389434610B9526d2f21b);
    IERC20 constant t1 = IERC20(0x330a1Ac45690b9ae1176389434610b9526D2f21B);

    function buy(uint amt) public {
        t1.transferFrom(msg.sender, address(this), amt);
        t0.transfer(msg.sender, amt);
    }
}
