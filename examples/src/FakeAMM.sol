// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract FakeAMM {
    uint beur;
    uint bdkk;
    
    function swap0() payable public {
    unchecked {
        uint amteur = msg.value;
        uint amtdkk = amteur * bdkk / (beur + amteur);

        beur += amteur;
        bdkk -= amtdkk;
        
        (bool succ,) = msg.sender.call{value: amtdkk}("");
        require(succ);
    }
    }

    function swap1() payable public {
    unchecked {
        uint amtdkk = msg.value;
        uint amteur = amtdkk * beur / (bdkk + amtdkk);
        
        beur -= amteur;
        bdkk += amtdkk;

        (bool succ,) = msg.sender.call{value: amteur}("");
        require(succ);
    }
    }
}
