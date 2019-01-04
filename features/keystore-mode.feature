Feature: Keystore Mode
  In order to manage a keystore
  As a user
  I want to do something

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
      .+PrivateKeyEntry[ ]+ca
      .+PrivateKeyEntry[ ]+root
      """

  Scenario: Adding a keypair to an existing keystore
    Given keystore "/tmp/keystore.jks" with password "insecure" and these keys:
      | alias | subject       |
      | root  | CN=root, C=US |
    When I create a keypair with alias "ca" and subject "CN=me, C=US" in keystore "/tmp/keystore.jks" with password "insecure"
    Then I should be in buffer "/tmp/keystore.jks"
    And I should see pattern:
      """
      .+PrivateKeyEntry[ ]+ca
      .+PrivateKeyEntry[ ]+root
      """
