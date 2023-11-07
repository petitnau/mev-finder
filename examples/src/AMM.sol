// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

import "./IERC20.sol";

contract AMM {
    IERC20 constant t0 = IERC20(0x230A1AC45690B9Ae1176389434610B9526d2f21b);
    IERC20 constant t1 = IERC20(0x330a1Ac45690b9ae1176389434610b9526D2f21B);

    uint r0;
    uint r1;

    function addliq(uint amt0, uint amt1) public {
        t0.transferFrom(msg.sender, address(this), amt0);
        t1.transferFrom(msg.sender, address(this), amt1);

        uint b0 = t0.balanceOf(address(this));
        uint b1 = t1.balanceOf(address(this));
        require(b0 * (b1 - amt1) == b1 *  (b0 - amt0));
    }

    function swap0(uint amt0, uint min1) public {
        t0.transferFrom(msg.sender, address(this), amt0);
        
        uint send1 = amt0 * t0.balanceOf(address(this));
        require(send1 >= min1 && send1 < t1.balanceOf(address(this)));
        t1.transfer(msg.sender, send1);
    }

    function swap1(uint amt1, uint min0) public {
        t1.transferFrom(msg.sender, address(this), amt1);
        
        uint send0 = amt1 * t1.balanceOf(address(this));
        require(send0 >= min0 && send0 < t0.balanceOf(address(this)));
        t0.transfer(msg.sender, send0);
    }
}
