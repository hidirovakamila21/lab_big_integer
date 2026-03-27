#include "big_integer.h"
#include <algorithm>
#include <cctype>
#include <string>
BigInteger::BigInteger() {
    digits_.push_back(0);
    negative_ = false;
}
BigInteger::BigInteger(int value) {
    if (value == 0) {
        digits_.push_back(0);
        negative_ = false;
        return;
    }
    if (value < 0) {
        negative_ = true;
        value = -value;
    } else {
        negative_ = false;
    }
    while (value > 0) {
        int digit = value % 10;
        digits_.push_back(digit);
        value = value / 10;
    }
}
BigInteger::BigInteger(long long value) {
    if (value == 0) {
        digits_.push_back(0);
        negative_ = false;
        return;
    }
    if (value < 0) {
        negative_ = true;
        value = -value;
    } else {
        negative_ = false;
    }
    while (value > 0) {
        int digit = value % 10;
        digits_.push_back(digit);
        value = value / 10;
    }
}
BigInteger::BigInteger(const std::string& str) {
    if (str.empty()) {
        digits_.push_back(0);
        negative_ = false;
        return;
    }
    int start = 0;
    if (str[0] == '-') {
        negative_ = true;
        start = 1;
    } else {
        negative_ = false;
        start = 0;
    }
    bool all_zeros = true;
    for (int i = start; i < (int)str.length(); i++) {
        if (str[i] != '0') {
            all_zeros = false;
            break;
        }
    }
    if (all_zeros) {
        digits_.push_back(0);
        negative_ = false;
        return;
    }
    for (int i = (int)str.length() - 1; i >= start; i--) {
        char c = str[i];
        if (c < '0' || c > '9') {
            digits_.push_back(0);
            negative_ = false;
            return;
        }
        int digit = c - '0';
        digits_.push_back(digit);
    }
    while (digits_.size() > 1 && digits_.back() == 0) {
        digits_.pop_back();
    }
}
BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    if (negative_ == false && rhs.negative_ == false) {
        BigInteger result;
        result.negative_ = false;
        int carry = 0;
        int max_len = digits_.size();
        if (rhs.digits_.size() > max_len) {
            max_len = rhs.digits_.size();
        }
        for (int i = 0; i < max_len || carry != 0; i++) {
            int sum = carry;
            if (i < (int)digits_.size()) {
                sum = sum + digits_[i];
            }
            if (i < (int)rhs.digits_.size()) {
                sum = sum + rhs.digits_[i];
            }
            int digit = sum % 10;
            carry = sum / 10;
            result.digits_.push_back(digit);
        }
        return result;
    }
    else if (negative_ == true && rhs.negative_ == true) {
        BigInteger result;
        result.negative_ = true;
        int carry = 0;
        int max_len = digits_.size();
        if (rhs.digits_.size() > max_len) {
            max_len = rhs.digits_.size();
        }
        for (int i = 0; i < max_len || carry != 0; i++) {
            int sum = carry;
            if (i < (int)digits_.size()) {
                sum = sum + digits_[i];
            }
            if (i < (int)rhs.digits_.size()) {
                sum = sum + rhs.digits_[i];
            }
            int digit = sum % 10;
            carry = sum / 10;
            result.digits_.push_back(digit);
        }
        return result;
    }
    else if (negative_ == true && rhs.negative_ == false) {
        BigInteger temp = *this;
        temp.negative_ = false;
        return rhs - temp;
    }
    else {
        BigInteger temp = rhs;
        temp.negative_ = false;
        return *this - temp;
    }
}
BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    if (negative_ == false && rhs.negative_ == false) {
        if (*this == rhs) {
            return BigInteger(0);
        }
        bool swap = false;
        const BigInteger* bigger = this;
        const BigInteger* smaller = &rhs;
        if (digits_.size() < rhs.digits_.size()) {
            swap = true;
            bigger = &rhs;
            smaller = this;
        }
        else if (digits_.size() == rhs.digits_.size()) {
            for (int i = (int)digits_.size() - 1; i >= 0; i--) {
                if (digits_[i] < rhs.digits_[i]) {
                    swap = true;
                    bigger = &rhs;
                    smaller = this;
                    break;
                }
                else if (digits_[i] > rhs.digits_[i]) {
                    break;
                }
            }
        }
        BigInteger result;
        if (swap) {
            result.negative_ = true;
        } else {
            result.negative_ = false;
        }
        int borrow = 0;
        for (int i = 0; i < (int)bigger->digits_.size(); i++) {
            int diff = bigger->digits_[i] - borrow;
            if (i < (int)smaller->digits_.size()) {
                diff = diff - smaller->digits_[i];
            }
            if (diff < 0) {
                diff = diff + 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits_.push_back(diff);
        }
        while (result.digits_.size() > 1 && result.digits_.back() == 0) {
            result.digits_.pop_back();
        }
        return result;
    }
    else if (negative_ == true && rhs.negative_ == true) {
        BigInteger temp1 = *this;
        temp1.negative_ = false;
        BigInteger temp2 = rhs;
        temp2.negative_ = false;
        return temp2 - temp1;
    }
    else if (negative_ == false && rhs.negative_ == true) {
        BigInteger temp = rhs;
        temp.negative_ = false;
        return *this + temp;
    }
    else {
        BigInteger temp = *this;
        temp.negative_ = false;
        BigInteger result = temp + rhs;
        result.negative_ = true;
        return result;
    }
}
BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    if (is_zero() || rhs.is_zero()) {
        return BigInteger(0);
    }
    BigInteger result;
    result.digits_.resize(digits_.size() + rhs.digits_.size(), 0);
    for (int i = 0; i < (int)digits_.size(); i++) {
        int carry = 0;
        for (int j = 0; j < (int)rhs.digits_.size() || carry != 0; j++) {
            long long mul = result.digits_[i + j];
            if (j < (int)rhs.digits_.size()) {
                mul = mul + (long long)digits_[i] * rhs.digits_[j];
            }
            mul = mul + carry;
            result.digits_[i + j] = mul % 10;
            carry = mul / 10;
        }
    }
    while (result.digits_.size() > 1 && result.digits_.back() == 0) {
        result.digits_.pop_back();
    }
    if (negative_ != rhs.negative_) {
        result.negative_ = true;
    } else {
        result.negative_ = false;
    }
    return result;
}
BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    if (rhs.is_zero()) {
        return BigInteger(0);
    }
    if (is_zero()) {
        return BigInteger(0);
    }
    bool result_negative = false;
    if (negative_ != rhs.negative_) {
        result_negative = true;
    }
    BigInteger dividend = *this;
    dividend.negative_ = false;
    BigInteger divisor = rhs;
    divisor.negative_ = false;
    if (dividend < divisor) {
        return BigInteger(0);
    }
    BigInteger result;
    result.digits_.clear();
    result.negative_ = false;
    BigInteger current;
    for (int i = dividend.digits_.size() - 1; i >= 0; i--) {
        if (current.is_zero() && dividend.digits_[i] == 0) {
            if (!result.digits_.empty()) {
                result.digits_.push_back(0);
            }
            continue;
        }
        if (current.is_zero()) {
            current.digits_.clear();
            current.digits_.push_back(dividend.digits_[i]);
        } else {
            current.digits_.insert(current.digits_.begin(), dividend.digits_[i]);
        }
        while (current.digits_.size() > 1 && current.digits_.back() == 0) {
            current.digits_.pop_back();
        }
        int count = 0;
        while (true) {
            bool less = false;
            if (current.digits_.size() < divisor.digits_.size()) {
                less = true;
            }
            else if (current.digits_.size() > divisor.digits_.size()) {
                less = false;
            }
            else {
                for (int k = current.digits_.size() - 1; k >= 0; k--) {
                    if (current.digits_[k] < divisor.digits_[k]) {
                        less = true;
                        break;
                    }
                    else if (current.digits_[k] > divisor.digits_[k]) {
                        less = false;
                        break;
                    }
                }
            }
            if (less) {
                break;
            }
            int borrow = 0;
            for (int k = 0; k < (int)current.digits_.size(); k++) {
                int diff = current.digits_[k] - borrow;
                if (k < (int)divisor.digits_.size()) {
                    diff = diff - divisor.digits_[k];
                }
                if (diff < 0) {
                    diff = diff + 10;
                    borrow = 1;
                } else {
                    borrow = 0;
                }
                current.digits_[k] = diff;
            }
            while (current.digits_.size() > 1 && current.digits_.back() == 0) {
                current.digits_.pop_back();
            }
            count = count + 1;
        }
        result.digits_.push_back(count);
    }
    std::reverse(result.digits_.begin(), result.digits_.end());
    while (result.digits_.size() > 1 && result.digits_.back() == 0) {
        result.digits_.pop_back();
    }
    if (result.digits_.empty()) {
        result.digits_.push_back(0);
    }
    result.negative_ = result_negative;
    if (result.is_zero()) {
        result.negative_ = false;
    }
    return result;
}
BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger quotient = *this / rhs;
    BigInteger product = quotient * rhs;
    BigInteger result = *this - product;
    if (result.is_zero()) {
        result.negative_ = false;
        return result;
    }
    if (negative_ == true && result.negative_ == false && !result.is_zero()) {
        result.negative_ = true;
    }
    if (negative_ == false && result.negative_ == true) {
        result.negative_ = false;
    }
    return result;
}
BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    *this = *this + rhs;
    return *this;
}
BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this = *this - rhs;
    return *this;
}
BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    *this = *this * rhs;
    return *this;
}
BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    *this = *this / rhs;
    return *this;
}
BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    *this = *this % rhs;
    return *this;
}
BigInteger BigInteger::operator-() const {
    BigInteger result = *this;
    if (!is_zero()) {
        result.negative_ = !negative_;
    }
    return result;
}
BigInteger& BigInteger::operator++() {
    *this = *this + BigInteger(1);
    return *this;
}
BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    *this = *this + BigInteger(1);
    return old;
}
BigInteger& BigInteger::operator--() {
    *this = *this - BigInteger(1);
    return *this;
}
BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    *this = *this - BigInteger(1);
    return old;
}
bool BigInteger::operator==(const BigInteger& rhs) const {
    if (is_zero() && rhs.is_zero()) {
        return true;
    }
    if (negative_ != rhs.negative_) {
        return false;
    }
    if (digits_.size() != rhs.digits_.size()) {
        return false;
    }
    for (int i = 0; i < (int)digits_.size(); i++) {
        if (digits_[i] != rhs.digits_[i]) {
            return false;
        }
    }
    return true;
}
bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}
bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ == true && rhs.negative_ == false) {
        return true;
    }
    if (negative_ == false && rhs.negative_ == true) {
        return false;
    }
    if (negative_ == true && rhs.negative_ == true) {
        BigInteger temp1 = *this;
        temp1.negative_ = false;
        BigInteger temp2 = rhs;
        temp2.negative_ = false;
        if (temp2 < temp1) {
            return true;
        } else {
            return false;
        }
    }
    if (digits_.size() != rhs.digits_.size()) {
        if (digits_.size() < rhs.digits_.size()) {
            return true;
        } else {
            return false;
        }
    }
    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        if (digits_[i] != rhs.digits_[i]) {
            if (digits_[i] < rhs.digits_[i]) {
                return true;
            } else {
                return false;
            }
        }
    }
    return false;
}
bool BigInteger::operator>(const BigInteger& rhs) const {
    if (*this == rhs) {
        return false;
    }
    if (*this < rhs) {
        return false;
    }
    return true;
}
bool BigInteger::operator<=(const BigInteger& rhs) const {
    if (*this == rhs) {
        return true;
    }
    if (*this < rhs) {
        return true;
    }
    return false;
}
bool BigInteger::operator>=(const BigInteger& rhs) const {
    if (*this == rhs) {
        return true;
    }
    if (*this > rhs) {
        return true;
    }
    return false;
}
std::string BigInteger::to_string() const {
    if (is_zero()) {
        return "0";
    }
    std::string result;
    if (negative_) {
        result = result + "-";
    }
    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        char digit_char = '0' + digits_[i];
        result = result + digit_char;
    }
    return result;
}
bool BigInteger::is_zero() const {
    if (digits_.size() == 1 && digits_[0] == 0) {
        return true;
    }
    return false;
}
bool BigInteger::is_negative() const {
    return negative_;
}
BigInteger::operator bool() const {
    return !is_zero();
}
std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    os << value.to_string();
    return os;
}
std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string str;
    is >> str;
    value = BigInteger(str);
    return is;
}
