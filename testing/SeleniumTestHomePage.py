# -*- coding: utf-8 -*-
"""
Created on Fri Dec 30 13:13:06 2016

@author: David Smyth
"""
#http://www.techbeamers.com/selenium-python-test-suite-unittest/
from selenium import webdriver
import unittest
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.errorhandler import NoSuchElementException
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
#==============================================================================
#browser = webdriver.Chrome('/home/user15/Downloads/David/chromedriver')
#url='localhost:6060'
#browser.get(url)
#browser.find_elements_by_xpath("//a [@data-value='Download Report']")
#t=browser.find_element_by_id('bingo')
#t.is_selected()
#ids = browser.find_elements_by_xpath('//*[@id]')
#browser.find_element_by_data_attribute('a href')
#for ii in ids:
    #print ii.tag_name
#    print(ii.get_attribute('id'))
#for ii in ids:
#    print(ii.get_attribute('class'))
#browser.find_element_by_class_name('progress').text
#     print(ii.tag_name)
# #browser.find_element_by_css_selector('.input-group-btn:first-child ').click()
#browser.find_element_by_id('file1').send_keys("C:\\Users\\Marion\\Downloads\\GPSDataDateModified.csv")
# 

#Ideally have a test that first checks that all elements in hompage are present when visited
#Then have separate tests for the functionality of each of the widgets

class AppTest(unittest.TestCase):
    '''A class used to test shiny sports app'''
    
    #make this class method so that it isn't run multiple times    
    @classmethod    
    def setUp(inst):
        url='localhost:6060'
        inst.driver=webdriver.Chrome('/home/user15/Downloads/David/chromedriver')
        inst.driver.get(url)
        
    def test_title(self):
        '''Small sample test to ensure title is present and correct'''
        self.assertEqual(self.driver.find_element_by_class_name('title').text,'Sports Analysis Dashboard','Page title is not Menu:')

    #these should make greater use of xpath
    #Ids in shiny are unique due to namespace issues so going by ID seems like a good strategy for now
    def test_dropbox_test_box(self):
        self.assertTrue(self.elementExists(By.ID,'dropboxDownload'))
    
    def test_dropbox_file_selection(self):
        self.assertTrue(self.elementExists(By.ID,'dropboxFileSelection'))
    
    def test_local_file_upload_exists(self):
@        self.assertTrue(self.elementExists(By.XPATH,"//input [@id='file1']"))
        
    def test_select_player_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//label[@for='getname']"))
        self.assertTrue(self.elementExists(By.ID,'getname'))
        
    def test_select_variable_exists(self):
        self.assertTrue(self.elementExists(By.ID,'getVariable'))
    
    def test_select_comparison_player_exists(self):
        self.assertTrue(self.elementExists(By.ID,'getComparisonName'))
        
    def test_drill_annotater(self):
        self.assertTrue(self.elementExists(By.ID,'annotateDrills'))
        
    def player_summary_exists(self):
        self.assertTrue(self.elementExists(By.ID,'PlayerSummary'))
         #self.assertTrue(self.elementExists(By.)
    def annotate_all_drills_checkbox_exists(self):
        self.assertTrue(self.elementExists(By.ID,'annotateAllDrills'))
    def test_download_report_tab_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//a [@data-value='Download Report']"))
        
    def test_summary_tab_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//a [@data-value='Summary']"))
    
    def test_barplot_tab_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//a [@data-value='Barplot']"))
    
    def test_rankings_tab_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//a [@data-value='Rankings']"))
        
    def test_plot_tab_exists(self):
        self.assertTrue(self.elementExists(By.XPATH,"//a [@data-value='Plot']"))
    
    def test_dygraph_exists(self):
        self.assertTrue(self.elementExists(By.ID,'dygraph'))


        #self.assertTrue(

    #def test_local_file_upload_exists(self):
    #    '''check that the page has the ability to upload a local file'''
    #    ids = self.driver.find_elements_by_xpath('//*[@id]')
    #    self.assertIn('file1',ids)
    
    #def test_local_file_upload(self):
    #    '''check that the page allows the user to upload a local csv file'''
    #    self.driver.find_element_by_id('file1').send_keys("C:\\Users\\Marion\\Downloads\\GPSDataDateModified.csv")
    #    self.assertEqual(self.driver.find_element_by_class_name('progress'),'Upload complete','When a file upload is attempted, the upload box does not display Upload complete')
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    