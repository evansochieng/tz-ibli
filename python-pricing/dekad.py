# Create a class that returns a dekad from date

class Dekad:
    
    # initialize variable date
    def __init__(self, date):
        self.date = date
        
    # Create a function to map date to respective dekad
    def map_date_to_dekad(self):
        # check the month 
        if self.date.month == 1:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 1
            elif self.date.day in range(11, 21):
                dekad = 2
            elif self.date.day in range(21, 32):
                dekad = 3
        elif self.date.month == 2:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 4
            elif self.date.day in range(11, 21):
                dekad = 5
            elif self.date.day in range(21, 32):
                dekad = 6
        elif self.date.month == 3:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 7
            elif self.date.day in range(11, 21):
                dekad = 8
            elif self.date.day in range(21, 32):
                dekad = 9
        elif self.date.month == 4:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 10
            elif self.date.day in range(11, 21):
                dekad = 11
            elif self.date.day in range(21, 32):
                dekad = 12
        elif self.date.month == 5:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 13
            elif self.date.day in range(11, 21):
                dekad = 14
            elif self.date.day in range(21, 32):
                dekad = 15
        elif self.date.month == 6:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 16
            elif self.date.day in range(11, 21):
                dekad = 17
            elif self.date.day in range(21, 32):
                dekad = 18
        elif self.date.month == 7:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 19
            elif self.date.day in range(11, 21):
                dekad = 20
            elif self.date.day in range(21, 32):
                dekad = 21
        elif self.date.month == 8:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 22
            elif self.date.day in range(11, 21):
                dekad = 23
            elif self.date.day in range(21, 32):
                dekad = 24
        elif self.date.month == 9:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 25
            elif self.date.day in range(11, 21):
                dekad = 26
            elif self.date.day in range(21, 32):
                dekad = 27
        elif self.date.month == 10:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 28
            elif self.date.day in range(11, 21):
                dekad = 29
            elif self.date.day in range(21, 32):
                dekad = 30
        elif self.date.month == 11:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 31
            elif self.date.day in range(11, 21):
                dekad = 32
            elif self.date.day in range(21, 32):
                dekad = 33
        elif self.date.month == 12:
            # check the range where the day falls and get dekad
            if self.date.day in range(1, 11):
                dekad = 34
            elif self.date.day in range(11, 21):
                dekad = 35
            elif self.date.day in range(21, 32):
                dekad = 36
                
        # return the dekad
        return dekad