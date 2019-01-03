Feature: Keystore Mode
  In order to manage a keystore
  As a user
  I want to do something

  Scenario: Create a keystore
    Given I create a new keystore "/tmp/keystore.jks" with subject "CN=me, C=US" and password "insecure"
    When I open keystore "/tmp/keystore.jks" with password "insecure"
    When I switch to buffer "/tmp/keystore.jks"
    Then I should be in buffer "/tmp/keystore.jks"

