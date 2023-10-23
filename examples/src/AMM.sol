// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

import "./IERC20.sol";

contract AMM {
    uint beur;
    uint bdkk;
    
    IERC20 constant eur = 
        IERC20(0x0000000000000000000000000000000000000001);
    IERC20 constant dkk = 
        IERC20(0x0000000000000000000000000000000000000002);

    function swap0(uint amteur) public { unchecked {
        uint amtdkk = amteur * bdkk / (beur + amteur);

        bdkk -= amtdkk;
        beur += amteur;
        
        eur.transferFrom(msg.sender, address(this), amteur);
        dkk.transfer(msg.sender, amtdkk);
    }}

    function swap1(uint amtdkk) public { unchecked {
        uint amteur = amtdkk * beur / (bdkk + amtdkk);
        
        beur -= amteur;
        bdkk += amtdkk;

        dkk.transferFrom(msg.sender, address(this), amtdkk);
        eur.transfer(msg.sender, amteur);
    }}
}
