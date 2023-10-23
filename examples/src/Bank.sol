// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Bank {
    mapping (address => uint) acct;
    
    function deposit() public payable {
        require(msg.value > 0);
        acct[msg.sender] += msg.value;
    }

    function transfer(uint amt, address to) public {
        require(acct[msg.sender] >= amt && amt > 0);
        acct[msg.sender] -= amt;
        acct[to]         += amt;
    }
    
    function withdraw(uint amt) public{
        unchecked {
            
        (bool succ,) = msg.sender.call{value: amt}("");
        require(succ);
        }
    }
}
