# -*- coding: utf-8 -*-
"""
Created on Sat Jan  7 23:33:06 2017

@author: user15
"""

from selenium import webdriver
import unittest
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.errorhandler import NoSuchElementException
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
#==============================================================================
# import webbrowser
#webbrowser.open('https://docs.python.org/3.5/library/webbrowser.html')
#browser = webdriver.Chrome('/home/user15/Downloads/David/chromedriver')
# url='localhost:6060'
# browser.get(url)
# len(browser.window_handles)
# browser.find_element_by_xpath("//input [@id='dropboxDownload']").click()
#browser.find_element_by_xpath("//input [@id='file1']").send_keys("/home/user15/Dropbox/FinalYearProject/GPSDataDateModified.csv")
#browser.find_element_by_class_name('progress').text,'Upload complete','When a file upload is attempted, the upload box does not display Upload complete')

# /div [@class=selectize-input items has-options full has-items]')

# browser.switch_to.window(browser.window_handles[0])
# len(browser.window_handles)
# browser.switch_to.active_element
#==============================================================================
#browser.page_source


class UploadTest(unittest.TestCase):
    '''A class used to test shiny sports app'''
    
    #make this class method so that it isn't run multiple times    
    @classmethod    
    def setUp(inst):
        url='localhost:6060'
        inst.driver=webdriver.Chrome('/home/user15/Downloads/David/chromedriver')
        inst.driver.get(url)
        

    def test_local_file_upload(self):
        '''check that the page has the ability to upload a local file'''
        self.driver.find_element_by_xpath("//input [@id='file1']").send_keys("/home/user15/Dropbox/FinalYearProject/GPSDataDateModified.csv")
        #come back to this
        time.sleep(8)
        self.assertEqual(self.driver.find_element_by_class_name('progress').text,'Upload complete','When a file upload is attempted, the upload box does not display Upload complete')
    
        
#     def test_dropbox_checkbox_populates_select_input(self):
#         '''check that the page has the ability to upload a local file'''
#         #click dropbox checkbox
#         self.driver.find_element_by_xpath("//input [@id='dropboxDownload']").click()
#         self.assertTrue(self.elementExists(By.XPATH,'//select [@id=dropboxFileSelection] //div [@class=selectize-input items has-options full has-items]'))
#==============================================================================
        
    def elementExists(self, how, what):
        """
        Helper method to confirm the presence of an element on page
        how: By locator type
        what: locator value
        """
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException: return False
        return True
        
    
    @classmethod
    def tearDown(inst):
        inst.driver.quit()
    
if __name__ == '__main__':
    unittest.main()