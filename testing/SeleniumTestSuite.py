# -*- coding: utf-8 -*-
"""
Created on Fri Dec 30 19:32:41 2016

@author: David Smyth
"""

import unittest
from SeleniumTestHomePage import AppTest
from SeleniumTestUploadSuccess import UploadTest
#from SeleniumPythonMultipleTests import HomePageTest
 
# get all tests from SearchText and HomePageTest class
app_test = unittest.TestLoader().loadTestsFromTestCase(AppTest)
upload_test= unittest.TestLoader().loadTestsFromTestCase(UploadTest)

 
# create a test suite combining search_text and home_page_test
test_suite = unittest.TestSuite([app_test,upload_test])
 
# run the suite
unittest.TextTestRunner(verbosity=2).run(test_suite)