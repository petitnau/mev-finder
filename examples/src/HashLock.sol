// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract HashLock {
    bytes32 commit;

    function deposit (bytes32 _commit) public payable {
        require(commit == 0);
        commit = _commit;
    }
    
    function withdraw (bytes32 secret) public {
        require (keccak256(abi.encodePacked(secret)) == commit);
        (bool succ,) = msg.sender.call{value: 10}("");
        require(succ);
    }
}
