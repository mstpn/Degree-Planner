"""
This is a script to run all of the tests in the Python Test Plan document

To run the tests, make sure that you are in the parent directory of /python (i.e. the root project folder)
and run the following command:
    python3 run_tests.py
"""

from graph import build_graph
import student as std
import schedule as shed
import unittest
from unittest import TestCase


def build_student(name):
    test_path = "data/input/test/python/"
    student_input_file = test_path + str(name) + ".json"
    return std.Student(filename=student_input_file)


class TestOOB(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for OOB\n')

    def test_zero_semesters(self):
        student = build_student(1)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_neg_semesters(self):
        student = build_student(2)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_zero_courses(self):
        student = build_student(3)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_neg_courses(self):
        student = build_student(4)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)


class TestBasicFail(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for Basic Fail\n')

    def test_overconstrained_1(self):
        student = build_student(5)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_overconstrained_2(self):
        student = build_student(6)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_overconstrained_3(self):
        student = build_student(7)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)


class TestBasicPass(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\nRunning tests for Basic Pass\n')

    def test_empty_easy(self):
        student = build_student(8)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_empty_med(self):
        student = build_student(9)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_empty_hard(self):
        student = build_student(10)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)


class TestPreviousCourses(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for Previous Courses\n')

    def test_first_year_taken(self):
        student = build_student(11)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)
        self.assertLessEqual(len(degree), 6)

    def test_second_year_taken(self):
        student = build_student(12)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)
        self.assertLessEqual(len(degree), 4)

    def test_third_year_taken(self):
        student = build_student(13)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)
        self.assertLessEqual(len(degree), 2)

    def test_all_taken(self):
        student = build_student(23)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)


class TestOptions(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for Options\n')

    def test_junior_options(self):
        student = build_student(14)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_senior_no_junior_short(self):
        student = build_student(15)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_senior_no_junior_long(self):
        student = build_student(16)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_senior_options(self):
        student = build_student(17)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_cognate(self):
        student = build_student(18)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_invalid_junior_options(self):
        student = build_student(24)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)
    
    def test_invalid_senior_options(self):
        student = build_student(25)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_invalid_cognate(self):
        student = build_student(26)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)

    def test_invalid_options_and_cognate(self):
        student = build_student(27)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertFalse(success)


class TestFullDegree(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for Full Degree\n')

    def test_full_degree(self):
        student = build_student(19)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_full_degree_different_options(self):
        student = build_student(20)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

    def test_full_degree_different_cognate(self):
        student = build_student(21)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

class TestPedantic(TestCase):
    @classmethod
    def setUpClass(cls):
        print('\n\n\nRunning tests for Pedantic\n')

    def test_pedantic(self):
        student = build_student(22)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        self.assertTrue(success)

if __name__ == '__main__':
    # Build graphs (only need to do this once)
    print('building graph')
    build_graph()

    unittest.main(verbosity=2)
