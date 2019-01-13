Feature: Keystore Mode
  In order to manage a keystore
  As a user
  I want to manage the contents in emacs

  Scenario: Creating a keypair in a new keystore
    Given keystore "/tmp/keystore.jks" does not exist
    And buffer "/tmp/keystore.jks" does not exist
    When I create a keypair with alias "root" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      PrivateKeyEntry[ ]+root
      """

  Scenario: Opening an existing keystore
    Given buffer "/tmp/keystore.jks" does not exist
    And I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an existing keystore
    Given buffer "/tmp/keystore.jks" does not exist
    And keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "ca" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an opened keystore
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "ca" and subject "CN=me, C=US"
    Then I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Overwriting an existing keypair to an existing keystore
    Given buffer "/tmp/keystore.jks" does not exist
    And keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "root" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Marking a keystore entry for deletion
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    When I place the cursor before "ca"
    And I press "d"
    And I place the cursor before "root"
    And I press "d"
    Then I should see pattern:
      """
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Unmarking a keystore entry for deletion
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    When I place the cursor before "root"
    And I press "d"
    And I place the cursor before "ca"
    And I press "d"
    And I place the cursor before "root"
    And I press "d"
    Then I should see pattern:
      """
      [D][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Deleting a keystore entry
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    When I place the cursor before "ca"
    And I start an action chain
    And I press "d"
    And I press "x"
    And I press "y"
    And I execute the action chain
    Then I should not see pattern:
      """
      .+PrivateKeyEntry[ ]+ca
      """

  Scenario: Printing a certificate
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | ca    | CN=ca, C=US   |
    And buffer "*printcert: ca" does not exist
    When I place the cursor before "ca"
    And I press "p"
    Then buffer "*printcert: ca*" should contain:
      """
      Owner: CN=ca, C=US
      Issuer: CN=ca, C=US
      """

  Scenario: Exporting a certificate
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | ca    | CN=ca, C=US   |
    And buffer "ca.pem" does not exist
    When I place the cursor before "ca"
    And I press "e"
    Then buffer "ca.pem" should contain pattern:
      """
      -----BEGIN CERTIFICATE-----
      """

  Scenario: Listing the contents of a keystore
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    When I place the cursor before "ca"
    And I press "l"
    Then buffer "*Keystore details: /tmp/keystore.jks*" should contain pattern:
      """
      Your keystore contains 2 entries

      root, .+, PrivateKeyEntry,[ ]
      Certificate fingerprint (.+): .+
      ca, .+, PrivateKeyEntry,[ ]
      Certificate fingerprint (.+): .+
      """

  Scenario: Listing the contents of a keystore
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    When I place the cursor before "ca"
    And I press "v"
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
      Alias name: ca
      """
    And I should see:
      """
      Entry type: PrivateKeyEntry
      Certificate chain length: 1
      Certificate[1]:
      Owner: CN=root, C=US
      Issuer: CN=root, C=US
      """

  Scenario: Listing the contents of a keystore
    Given I open keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And buffer "*Keystore details: /tmp/keystore.jks*" does not exist
    When I place the cursor before "ca"
    And I press "r"
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
      Alias name: ca
      """
    And I should see:
      """
      -----BEGIN CERTIFICATE-----
      """
