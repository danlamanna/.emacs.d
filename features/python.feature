Feature: Python editing

  Background:
    Given I switch to buffer "*some-python-buffer*"
    And I activate "python-mode"

  # Scenario: Typing erroneous code
  #   When I insert:
  #   """
  #   import os

  #   """
  #   And I go to end of buffer
  #   Then I should have "2" overlays

  # Scenario: Adding documentation
  #   When I insert:
  #   """
  #   def some_func():

  #   """
  #   And I go to end of buffer
  #   And I press "C-c M-d"
  #   Then I should see:
  #   """
  #   FIXME! briefly describe function
  #   """

  Scenario: Expanding snippet
    When I clear the buffer
    And I insert:
    """
    ifm
    """
    And I go to end of buffer
    And I press "TAB"
    Then I should see:
    """
    __main__
    """

  # Scenario: Navigating methods
  #   When I clear the buffer
  #   And I insert:
  #   """
  #   import os
  #   os.path.expanduser
  #   """
  #   And I go to end of buffer
  #   And I call "jedi:goto-definition"
  #   Then I should be in file "ntpath.py"
