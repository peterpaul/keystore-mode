Feature: Keystore Mode
  In order to manage a keystore
  As a user
  I want to manage the contents in emacs

  Scenario: Creating an empty keystore
    Given file "/tmp/keystore.jks" does not exist
    And buffer "/tmp/keystore.jks" does not exist
    When I start an action chain
    And I press "M-x"
    And I type "keystore-empty"
    And I press "RET"
    And I type "/tmp/keystore.jks"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I execute the action chain
    Then I should be in buffer "/tmp/keystore.jks"
    And file "/tmp/keystore.jks" should exist
    And I should not see pattern:
      """
      .*PrivateKeyEntry.*
      """
    And I should not see pattern:
      """
      .*trustedCertEntry.*
      """

  Scenario: Creating an empty keystore when file already exist
    Given buffer "/tmp/keystore.jks" does not exist
    When I start an action chain
    And I press "M-x"
    And I type "keystore-empty"
    And I press "RET"
    And I type "/tmp/keystore.jks"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I execute the action chain, and capture errors
    Then I should see error message "File ’/tmp/keystore.jks’ already exists, not generating empty keystore."

  Scenario: Opening an existing keystore
    Given file "/tmp/keystore.jks" does not exist
    And buffer "/tmp/keystore.jks" does not exist
    And keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    When I start an action chain
    And I press "M-x"
    And I type "keystore-visit"
    And I press "RET"
    And I type "/tmp/keystore.jks"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I execute the action chain
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """
    And I should see pattern:
      """
      .*PrivateKeyEntry.*
      """

  Scenario: Creating a keypair in a new keystore
    Given file "/tmp/keystore.jks" does not exist
    And buffer "/tmp/keystore.jks" does not exist
    When I start an action chain
    And I press "M-x"
    And I type "keystore-genkeypair"
    And I press "RET"
    And I type "/tmp/keystore.jks"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I type "1024"
    And I press "RET"
    And I type "365"
    And I press "RET"
    And I type "root"
    And I press "RET"
    And I type "root"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I type "US"
    And I press "RET"
    And I execute the action chain
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an existing keystore
    Given buffer "/tmp/keystore.jks" does not exist
    When I create a keypair with alias "ca1" and subject "CN=ca1, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    And I start an action chain
    And I press "G"
    And I type "1024"
    And I press "RET"
    And I type "365"
    And I press "RET"
    And I type "ca1"
    And I press "RET"
    And I type "ca1"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I press "RET"
    And I type "US"
    And I press "RET"
    And I execute the action chain    
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca1
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an opened keystore
    When I create a keypair with alias "ca2" and subject "CN=ca2, C=US"
    Then I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca1
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Overwriting an existing keypair to an existing keystore
    When I create a keypair with alias "root" and subject "CN=root, C=US"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca1
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Marking a keystore entry for deletion
    When I place the cursor before "ca1"
    And I press "d"
    And I place the cursor before "root"
    And I press "d"
    Then I should see pattern:
      """
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca1
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Unmarking a keystore entry for deletion
    When I place the cursor before "root"
    And I press "d"
    Then I should see pattern:
      """
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca1
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Deleting a keystore entry
    When I press "x"
    Then I should not see pattern:
      """
      .+PrivateKeyEntry[ ]+ca1
      """
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Printing a certificate
    Given buffer "*printcert: ca2" does not exist
    And I am in buffer "/tmp/keystore.jks"
    When I place the cursor before "ca2"
    And I press "p"
    Then buffer "*printcert: ca2*" should contain:
      """
      Owner: CN=ca2, C=US
      Issuer: CN=ca2, C=US
      """

  Scenario: Clean printcert
    Given buffer "*printcert: ca2*" does not exist

  Scenario: Exporting a certificate
    Given buffer "ca2.pem" does not exist
    And I am in buffer "/tmp/keystore.jks"
    When I place the cursor before "ca2"
    And I press "e"
    Then buffer "ca2.pem" should contain pattern:
      """
      -----BEGIN CERTIFICATE-----
      """

  Scenario: Clean certificate buffer
    Given buffer "ca2.pem" does not exist

  Scenario: Listing the contents of a keystore
    Given buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    And I am in buffer "/tmp/keystore.jks"
    When I press "l"
    Then I should see:
      """
      Your keystore contains 2 entries
      """
    And I should see pattern:
      """
      root, .+, PrivateKeyEntry,[ ]
      Certificate fingerprint (.+): .+
      """
    And I should see pattern:
      """
      ca2, .+, PrivateKeyEntry,[ ]
      Certificate fingerprint (.+): .+
      """

  Scenario: Listing the contents of a keystore verbose
    Given buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    And I am in buffer "/tmp/keystore.jks"
    When I press "v"
    Then I should see:
      """
      Your keystore contains 2 entries
      """
    And I should see:
      """
      Alias name: root
      """
    And I should see:
      """
      Alias name: ca2
      """
    And I should see:
      """
      Entry type: PrivateKeyEntry
      Certificate chain length: 1
      Certificate[1]:
      Owner: CN=root, C=US
      Issuer: CN=root, C=US
      """

  Scenario: Listing the contents of a keystore with pems
    Given buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    And I am in buffer "/tmp/keystore.jks"
    When I press "r"
    Then I should see:
      """
      Your keystore contains 2 entries
      """
    And I should see:
      """
      Alias name: root
      """
    And I should see:
      """
      Alias name: ca2
      """
    And I should see:
      """
      -----BEGIN CERTIFICATE-----
      """

  Scenario: Cleanup keystore details buffer
    Given buffer "*Keystore details: /tmp/keystore.jks*" does not exist

  Scenario: Importing a certificate from a buffer
    Given buffer "*test certificate*" with contents:
      """
      -----BEGIN CERTIFICATE-----
      MIIE1zCCAr+gAwIBAgIEPbNE1jANBgkqhkiG9w0BAQwFADAcMQswCQYDVQQGEwJV
      UzENMAsGA1UEAxMEdGVzdDAeFw0xOTAxMTMyMDU0MDhaFw0yMDAxMTMyMDU0MDha
      MBwxCzAJBgNVBAYTAlVTMQ0wCwYDVQQDEwR0ZXN0MIICIjANBgkqhkiG9w0BAQEF
      AAOCAg8AMIICCgKCAgEAnJFK+tRPL2GgWoRFlmBZLPwj9NPWvYyFuY5Jb2K/ub2x
      cWX1ncMy7JAS4hoa54vsSR3H+aio9aTvNvWVaqKfOQ+hnS/G9vW1gQfOn0D8n4Lj
      0XfVeLg0xjE0oR/2g2S/Su4vw/9Nl0T+bu5t76sTumRGJCFAxbIzACLQm0u7QrHU
      MDsZhMiCVx7l1jWwxijkmspbq3U6iSKoaeHyEL0SXDpLM4YtqjuFxyNv1+NMjg5+
      fgGbh/QAt/FWzFAz2dJHCZaorXSk0u4H7EN9jtI1MBBEHM8tKqZ4nUOriFWvowUI
      hc4Zy7Ts/a9mjmemGSK6j/2PmKJ6KgPollti14tHz6QXSXn/GU1sBEYeGJ4Z9t3+
      rqMaW21BdCi1oiX/e2i/WOSy7mR9iPy/2grY6YtI/cT+EAYMulQ6W3tUV5C5zvbR
      r5R5MsOtGTNJ6d/CW3TQ16/qxO+0r3n/VxXKoD1IH7IfU08YymvSBsXA7h9nxyU4
      YW2pRqQO2fRsx94fbnghfe/4WcXx6M5D4483pJvKlCByELkKoZfrAo7XS+j5nMBu
      BGhemgnlrLXUMHjkibvc6cVoP7G0HIo9HzS9jxsjYRbtp+O2LFgeI6ppPhP+m9Kr
      E1PaAISzKzYZAgu9bTN48m+7dbBBD2P1yuV9LR9DvPwaJhu9wyEgRzgb4cE4GXEC
      AwEAAaMhMB8wHQYDVR0OBBYEFK63zl54Su95U/FYsTZvqjPHdDstMA0GCSqGSIb3
      DQEBDAUAA4ICAQAxpKMIc6IQyvBOt85A5hh2YmSdf/rpBL8/TwlSDOchQSndqQZK
      b61scNkTJitDCFHEJnCLYd9yi0B2DAQ3B14kkpIvM2ZxdDF/Qb9pBMBZZL2TdtPg
      BsQNGmJ7K3cR5kK4441Vi4MakxXENbIHu6dzp1qEfjs1/hIFME5bcl4geoTw26kj
      WizUMH7rkLXmGTyVaVvFm1/Gusk6/mNVAeZdVfO7hVF7Om03o3sEH9H6B8XIO9Ru
      y9XUDbYix8pk11uNJ4ZkcsPZFV7AdDKkPIUBHYE6ASUFJCRRBVCnqUnbMAMoJ+z4
      CRLqK4ZYI39JDqp347+23D5ajV4mcRBVVWi2lnnvDQmmaHGeftdqLZlNkS37jT7b
      wdJuusA+5zEUQBSCSjytxgw1k6/ELZaEFw3W6hX6w+xZmJFTd9KNL9qOC+GXMgKZ
      vqd3NBP2Svu9TWqAOjwPz68QOAvdYrgG6vLG350ou4eV+SKU2MWTjPsnWOE3oRyc
      UxC+wzObAotnI4jpKUF+dWbV/hinlZ7Ymi8JjtwniZn6rGPTy31xnaK2t982Oe8Z
      SBZwuobAjBF7rW55izQadoKbRAixzLl2eu7WX31Ngt3zRE1bhl8UKBmWjpu92/2q
      UTqFVKH6fSWT1wtfqzhZ1BNDbvkxXc23VR/vb61GfFWSYnRUbKjTTDk71A==
      -----END CERTIFICATE-----
      """
    And I am in buffer "/tmp/keystore.jks"
    When I import certificate "test-buffer" from buffer "*test certificate*"
    Then buffer "/tmp/keystore.jks" should contain pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-buffer
      """

  Scenario: Cleaning test certificate buffer
    Given buffer "*test certificate*" does not exist

  Scenario: Importing a certificate from a file
    Given file "/tmp/cert.pem" with contents:
      """
      -----BEGIN CERTIFICATE-----
      MIIE1zCCAr+gAwIBAgIEPbNE1jANBgkqhkiG9w0BAQwFADAcMQswCQYDVQQGEwJV
      UzENMAsGA1UEAxMEdGVzdDAeFw0xOTAxMTMyMDU0MDhaFw0yMDAxMTMyMDU0MDha
      MBwxCzAJBgNVBAYTAlVTMQ0wCwYDVQQDEwR0ZXN0MIICIjANBgkqhkiG9w0BAQEF
      AAOCAg8AMIICCgKCAgEAnJFK+tRPL2GgWoRFlmBZLPwj9NPWvYyFuY5Jb2K/ub2x
      cWX1ncMy7JAS4hoa54vsSR3H+aio9aTvNvWVaqKfOQ+hnS/G9vW1gQfOn0D8n4Lj
      0XfVeLg0xjE0oR/2g2S/Su4vw/9Nl0T+bu5t76sTumRGJCFAxbIzACLQm0u7QrHU
      MDsZhMiCVx7l1jWwxijkmspbq3U6iSKoaeHyEL0SXDpLM4YtqjuFxyNv1+NMjg5+
      fgGbh/QAt/FWzFAz2dJHCZaorXSk0u4H7EN9jtI1MBBEHM8tKqZ4nUOriFWvowUI
      hc4Zy7Ts/a9mjmemGSK6j/2PmKJ6KgPollti14tHz6QXSXn/GU1sBEYeGJ4Z9t3+
      rqMaW21BdCi1oiX/e2i/WOSy7mR9iPy/2grY6YtI/cT+EAYMulQ6W3tUV5C5zvbR
      r5R5MsOtGTNJ6d/CW3TQ16/qxO+0r3n/VxXKoD1IH7IfU08YymvSBsXA7h9nxyU4
      YW2pRqQO2fRsx94fbnghfe/4WcXx6M5D4483pJvKlCByELkKoZfrAo7XS+j5nMBu
      BGhemgnlrLXUMHjkibvc6cVoP7G0HIo9HzS9jxsjYRbtp+O2LFgeI6ppPhP+m9Kr
      E1PaAISzKzYZAgu9bTN48m+7dbBBD2P1yuV9LR9DvPwaJhu9wyEgRzgb4cE4GXEC
      AwEAAaMhMB8wHQYDVR0OBBYEFK63zl54Su95U/FYsTZvqjPHdDstMA0GCSqGSIb3
      DQEBDAUAA4ICAQAxpKMIc6IQyvBOt85A5hh2YmSdf/rpBL8/TwlSDOchQSndqQZK
      b61scNkTJitDCFHEJnCLYd9yi0B2DAQ3B14kkpIvM2ZxdDF/Qb9pBMBZZL2TdtPg
      BsQNGmJ7K3cR5kK4441Vi4MakxXENbIHu6dzp1qEfjs1/hIFME5bcl4geoTw26kj
      WizUMH7rkLXmGTyVaVvFm1/Gusk6/mNVAeZdVfO7hVF7Om03o3sEH9H6B8XIO9Ru
      y9XUDbYix8pk11uNJ4ZkcsPZFV7AdDKkPIUBHYE6ASUFJCRRBVCnqUnbMAMoJ+z4
      CRLqK4ZYI39JDqp347+23D5ajV4mcRBVVWi2lnnvDQmmaHGeftdqLZlNkS37jT7b
      wdJuusA+5zEUQBSCSjytxgw1k6/ELZaEFw3W6hX6w+xZmJFTd9KNL9qOC+GXMgKZ
      vqd3NBP2Svu9TWqAOjwPz68QOAvdYrgG6vLG350ou4eV+SKU2MWTjPsnWOE3oRyc
      UxC+wzObAotnI4jpKUF+dWbV/hinlZ7Ymi8JjtwniZn6rGPTy31xnaK2t982Oe8Z
      SBZwuobAjBF7rW55izQadoKbRAixzLl2eu7WX31Ngt3zRE1bhl8UKBmWjpu92/2q
      UTqFVKH6fSWT1wtfqzhZ1BNDbvkxXc23VR/vb61GfFWSYnRUbKjTTDk71A==
      -----END CERTIFICATE-----
      """
    And I am in buffer "/tmp/keystore.jks"
    When I import certificate "test-file" from file "/tmp/cert.pem"
    Then buffer "/tmp/keystore.jks" should contain pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca2
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-buffer
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-file
      """

  Scenario: Changing an alias
    When I place the cursor before "ca2"
    And I start an action chain
    And I press "c"
    And I type "intermediate"
    And I press "RET"
    And I execute the action chain
    Then buffer "/tmp/keystore.jks" should contain pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+intermediate
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-buffer
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-file
      """

  Scenario: Generating a CSR
    Given I am in buffer "/tmp/keystore.jks"
    When I place the cursor before "intermediate"
    And I start an action chain
    And I press "s"
    And I type "/tmp/intermediate.csr"
    And I press "RET"
    And I execute the action chain
    Then file "/tmp/intermediate.csr" should exist

  Scenario: Creating a certificate
    Given I am in buffer "/tmp/keystore.jks"
    When I place the cursor before "root"
    And I start an action chain
    And I press "S"
    And I type "/tmp/intermediate.csr"
    And I press "RET"
    And I execute the action chain
    Then file "/tmp/intermediate.csr.pem" should exist    

  Scenario: Importing the generated certificate
    Given I am in buffer "/tmp/keystore.jks"
    When I import certificate "intermediate" from file "/tmp/intermediate.csr.pem"
    Then buffer "/tmp/keystore.jks" should contain pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+intermediate
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-buffer
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-file
      """

  Scenario: Validating the generated certificate
    Given I am in buffer "/tmp/keystore.jks"
    When I place the cursor before "intermediate"
    And I press "p"
    Then buffer "*printcert: intermediate*" should contain pattern:
      """
      Owner: CN=ca2, C=US
      Issuer: CN=root, C=US
      """

  Scenario: Importing another keystore
    Given buffer "/tmp/other-keystore.p12" does not exist
    And I open keystore "/tmp/other-keystore.p12" with password "insecure" and these keys:
      | alias | subject      |
      | key   | CN=key, C=US |
    And I am in buffer "/tmp/keystore.jks"
    When I start an action chain
    And I press "I"
    And I type "/tmp/other-keystore.p12"
    And I press "RET"
    And I type "insecure"
    And I press "RET"
    And I execute the action chain
    Then buffer "/tmp/keystore.jks" should contain pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+intermediate
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+key
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-buffer
      [ ][ ][0-9A-F]+[ ]+trustedCertEntry[ ]+test-file
      """
