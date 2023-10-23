// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

import "./IERC20.sol";

contract TokenWithdrawer {
    IERC20 constant eur = IERC20(0x0000000000000000000000000000000000000001);
    
    function withdraw() public {
        eur.transfer(msg.sender, 1);
    }
}
