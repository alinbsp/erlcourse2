
-record(accountDetails, {name, balance, pin}).
-record(account, {id, details=accountDetails#{}}).