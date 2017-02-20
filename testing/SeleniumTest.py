# -*- coding: utf-8 -*-
"""
Created on Fri Dec 30 13:13:06 2016

@author: David Smyth
"""

from selenium import webdriver
import unittest
from selenium.webdriver.common.keys import Keys
#==============================================================================
# Test Loader – It’s a Python class which loads test cases and suites created locally or from an external data source like a file. It releases a TestSuite object that carries those cases and suites.
# 
# Test Case – The TestCase class holds the test handlers and provides hooks for preparing each handler and for cleaning up after execution.
# 
# Test Suite – It acts as a container for grouping test cases. With the help of a test suite, you can combine a set of test cases representing specific functionalities of the application under test.
# 
# Test Runner – It provides a runnable interface for the execution of tests and delivers the results to the user. It can use channels like a GUI, a textual medium, or return a standard code to notify the results of test execution.
# 
# Test Report – This component organizes test results, display pass/fail status of the executed test cases. It even provides the details of steps, summary of overall run and the time lapsed in execution
#==============================================================================
#==============================================================================
browser = webdriver.Chrome('PathToChromeDriverExecutable')
url='localhost:6060'
browser.get(url)
ids = browser.find_elements_by_xpath('//*[@id]')
for ii in ids:
    #print ii.tag_name
    print(ii.get_attribute('id'))
for ii in ids:
    print(ii.get_attribute('class'))
browser.find_element_by_class_name('progress').text
#     print(ii.tag_name)
# #browser.find_element_by_css_selector('.input-group-btn:first-child ').click()
browser.find_element_by_id('file1').send_keys("PathToData")
# 
#==============================================================================

#Ideally have a test that first checks that all elements in hompage are present when visited
#Then have separate tests for the functionality of each of the widgets

class AppTest(unittest.TestCase):
    '''A class used to test shiny sports app'''
    url='localhost:6060'
    #make this class method so that it isn't run multiple times    
    @classmethod    
    def setUp(inst):
        inst.driver=webdriver.Chrome('PathToChromeDriverExecutable')
        inst.driver.get(url)
        
    def test_title(self):
        '''Small sample test to ensure title is present and correct'''
        self.assertEqual(self.driver.title,'Menu:','Page title is not Menu:')

    def test_local_file_upload_exists(self):
        '''check that the page has the ability to upload a local file'''
        ids = browser.find_elements_by_xpath('//*[@id]')
        self.assertIn('file1',ids)
    
    def test_local_file_upload(self):
        '''check that the page allows the user to upload a local csv file'''
        self.driver.find_element_by_id('file1').send_keys("PathToData")
        self.assertEqual(self.driver.find_element_by_class_name('progress'),'Upload complete','When a file upload is attempted, the upload box does not display Upload complete')
    
        
    @classmethod
    def tearDown(inst):
        inst.driver.quit()
    
if __name__ == '__main__':
    unittest.main()
