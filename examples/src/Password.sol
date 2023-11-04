// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract Password {
    uint password;

    function set(uint _password) public payable {
        require(msg.value > 10);
        password = _password;
    }

    function withdraw(uint amt) public {
        require(password == 34);
        (bool succ,) = msg.sender.call{value: amt}("");
        require(succ);
    }
}
