Feature: Keystore Mode
  In order to manage a keystore
  As a user
  I want to manage the contents in emacs

  Scenario: Creating a keypair in a new keystore
    Given keystore "/tmp/keystore.jks" does not exist
    When I create a keypair with alias "root" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      PrivateKeyEntry[ ]+root
      """

  Scenario: Opening an existing keystore
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    When I open keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an existing keystore
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "ca" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+ca
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Overwriting an existing keypair to an existing keystore
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "root" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      [ ][ ][0-9A-F]+[ ]+PrivateKeyEntry[ ]+root
      """

  Scenario: Marking a keystore entry for deletion
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And I open keystore "/tmp/keystore.jks" with password "insecure"
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
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And I open keystore "/tmp/keystore.jks" with password "insecure"
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
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And I open keystore "/tmp/keystore.jks" with password "insecure"
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
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
      | ca    | CN=ca, C=US   |
    And I open keystore "/tmp/keystore.jks" with password "insecure"
    When I place the cursor before "ca"
    And I press "p"
    Then buffer "*printcert: ca*" should contain:
      """
      Owner: CN=ca, C=US
      Issuer: CN=ca, C=US
      """
