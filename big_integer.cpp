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
        digits_.push_back(value % 10);
        value /= 10;
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
        digits_.push_back(value % 10);
        value /= 10;
    }
}

BigInteger::BigInteger(const std::string& str) {
    if (str.empty()) {
        digits_.push_back(0);
        negative_ = false;
        return;
    }
    int start = (str[0] == '-') ? 1 : 0;
    negative_ = (str[0] == '-');
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
        if (str[i] < '0' || str[i] > '9') {
            digits_.push_back(0);
            negative_ = false;
            return;
        }
        digits_.push_back(str[i] - '0');
    }
    while (digits_.size() > 1 && digits_.back() == 0) {
        digits_.pop_back();
    }
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    if (negative_ == rhs.negative_) {
        BigInteger result;
        result.negative_ = negative_;
        int carry = 0;
        int max_len = std::max(digits_.size(), rhs.digits_.size());
        for (int i = 0; i < max_len || carry != 0; i++) {
            int sum = carry;
            if (i < (int)digits_.size()) sum += digits_[i];
            if (i < (int)rhs.digits_.size()) sum += rhs.digits_[i];
            result.digits_.push_back(sum % 10);
            carry = sum / 10;
        }
        return result;
    }
    if (negative_ && !rhs.negative_) {
        BigInteger temp = *this;
        temp.negative_ = false;
        return rhs - temp;
    }
    BigInteger temp = rhs;
    temp.negative_ = false;
    return *this - temp;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    if (negative_ == rhs.negative_) {
        if (*this == rhs) return BigInteger(0);
        bool swap = false;
        const BigInteger* bigger = this;
        const BigInteger* smaller = &rhs;
        if (digits_.size() < rhs.digits_.size()) {
            swap = true;
            bigger = &rhs;
            smaller = this;
        } else if (digits_.size() == rhs.digits_.size()) {
            for (int i = (int)digits_.size() - 1; i >= 0; i--) {
                if (digits_[i] < rhs.digits_[i]) {
                    swap = true;
                    bigger = &rhs;
                    smaller = this;
                    break;
                } else if (digits_[i] > rhs.digits_[i]) {
                    break;
                }
            }
        }
        BigInteger result;
        result.negative_ = swap;
        int borrow = 0;
        for (int i = 0; i < (int)bigger->digits_.size(); i++) {
            int diff = bigger->digits_[i] - borrow;
            if (i < (int)smaller->digits_.size()) diff -= smaller->digits_[i];
            if (diff < 0) {
                diff += 10;
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
    if (!negative_ && rhs.negative_) {
        return *this + (-rhs);
    }
    if (negative_ && !rhs.negative_) {
        return -((-*this) + rhs);
    }
    BigInteger temp1 = *this, temp2 = rhs;
    temp1.negative_ = false;
    temp2.negative_ = false;
    return temp2 - temp1;
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    if (is_zero() || rhs.is_zero()) return BigInteger(0);
    BigInteger result;
    result.digits_.resize(digits_.size() + rhs.digits_.size(), 0);
    for (int i = 0; i < (int)digits_.size(); i++) {
        int carry = 0;
        for (int j = 0; j < (int)rhs.digits_.size() || carry != 0; j++) {
            long long mul = result.digits_[i + j];
            if (j < (int)rhs.digits_.size()) {
                mul += (long long)digits_[i] * rhs.digits_[j];
            }
            mul += carry;
            result.digits_[i + j] = mul % 10;
            carry = mul / 10;
        }
    }
    while (result.digits_.size() > 1 && result.digits_.back() == 0) {
        result.digits_.pop_back();
    }
    result.negative_ = (negative_ != rhs.negative_);
    return result;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    if (rhs.is_zero() || is_zero()) return BigInteger(0);
    bool result_negative = (negative_ != rhs.negative_);
    BigInteger dividend = *this, divisor = rhs;
    dividend.negative_ = false;
    divisor.negative_ = false;
    if (dividend < divisor) return BigInteger(0);
    BigInteger result, current;
    for (int i = dividend.digits_.size() - 1; i >= 0; i--) {
        if (current.is_zero() && dividend.digits_[i] == 0) {
            if (!result.digits_.empty()) result.digits_.push_back(0);
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
            } else if (current.digits_.size() > divisor.digits_.size()) {
                less = false;
            } else {
                for (int k = current.digits_.size() - 1; k >= 0; k--) {
                    if (current.digits_[k] < divisor.digits_[k]) {
                        less = true;
                        break;
                    } else if (current.digits_[k] > divisor.digits_[k]) {
                        less = false;
                        break;
                    }
                }
            }
            if (less) break;
            int borrow = 0;
            for (int k = 0; k < (int)current.digits_.size(); k++) {
                int diff = current.digits_[k] - borrow;
                if (k < (int)divisor.digits_.size()) diff -= divisor.digits_[k];
                if (diff < 0) {
                    diff += 10;
                    borrow = 1;
                } else {
                    borrow = 0;
                }
                current.digits_[k] = diff;
            }
            while (current.digits_.size() > 1 && current.digits_.back() == 0) {
                current.digits_.pop_back();
            }
            count++;
        }
        result.digits_.push_back(count);
    }
    std::reverse(result.digits_.begin(), result.digits_.end());
    while (result.digits_.size() > 1 && result.digits_.back() == 0) {
        result.digits_.pop_back();
    }
    result.negative_ = result_negative;
    if (result.is_zero()) result.negative_ = false;
    return result;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger result = *this - (*this / rhs) * rhs;
    if (result.is_zero()) {
        result.negative_ = false;
        return result;
    }
    if (negative_ && !result.negative_) result.negative_ = true;
    if (!negative_ && result.negative_) result.negative_ = false;
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
    if (!is_zero()) result.negative_ = !negative_;
    return result;
}

BigInteger& BigInteger::operator++() {
    *this = *this + 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    *this = *this + 1;
    return old;
}

BigInteger& BigInteger::operator--() {
    *this = *this - 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    *this = *this - 1;
    return old;
}

bool BigInteger::operator==(const BigInteger& rhs) const {
    if (is_zero() && rhs.is_zero()) return true;
    if (negative_ != rhs.negative_) return false;
    if (digits_.size() != rhs.digits_.size()) return false;
    for (int i = 0; i < (int)digits_.size(); i++) {
        if (digits_[i] != rhs.digits_[i]) return false;
    }
    return true;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ && !rhs.negative_) return true;
    if (!negative_ && rhs.negative_) return false;
    if (negative_ && rhs.negative_) {
        BigInteger t1 = *this, t2 = rhs;
        t1.negative_ = false;
        t2.negative_ = false;
        return t2 < t1;
    }
    if (digits_.size() != rhs.digits_.size()) {
        return digits_.size() < rhs.digits_.size();
    }
    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        if (digits_[i] != rhs.digits_[i]) {
            return digits_[i] < rhs.digits_[i];
        }
    }
    return false;
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return !(*this > rhs);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

std::string BigInteger::to_string() const {
    if (is_zero()) return "0";
    std::string result;
    if (negative_) result += "-";
    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        result += char('0' + digits_[i]);
    }
    return result;
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
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
