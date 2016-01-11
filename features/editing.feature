# Feature: General editing

#   Background:
#     Given I switch to buffer "*edit*"
#     And I clear the buffer

#   # This covers pending-delete-mode as well as expand-region
#   Scenario: Selecting and deleting text
#     Given I insert "foo-bar-baz"
#     And I go to word "bar"
#     And I press "C-q"
#     And I type "foo"
#     Then I should see:
#     """
#     foo-foo-baz
#     """

#   Scenario: Navigating words
#     Given I insert:
#     """
#     someCamelCaseWord
#     """
#     And I activate "prog-mode"
#     And I go to beginning of buffer
#     And I press "M-d"
#     Then I should see pattern "^CamelCaseWord"

#   Scenario: Editing multiple lines at once (multiple-cursors)
#     Given I insert:
#     """
#     Foo
#     Bar
#     Baz
#     """
#     And I go to beginning of buffer
#     And I press "C-c SPC"
#     And I go to line "3"
#     And I type "!"
#     Then I should see:
#     """
#     !Foo
#     !Bar
#     !Baz
#     """
