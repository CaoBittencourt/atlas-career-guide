// Hossein Moein
// September 25, 2018
/*
Copyright (c) 2019-2026, Hossein Moein
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* Neither the name of Hossein Moein and/or the DataFrame nor the
  names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Hossein Moein BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <iterator>
#include <new>
#include <utility>
#include <vector>

// ----------------------------------------------------------------------------

namespace hmdf
{

// This is a view that would make an already existing array of
// continuous memory look like an STL vector.
// It also gives you STL conformant iterators.
//
template<typename T, std::size_t A = 0>
class   VectorView {

public:

    static constexpr std::align_val_t   align_value { A };

    using value_type = T;
    using size_type =
        typename std::vector<
            T, typename allocator_declare<T, A>::type>::size_type;
    using pointer = value_type *;
    using const_pointer = const value_type *;
    using const_pointer_const = const value_type *const;
    using reference = value_type &;
    using const_reference = const value_type &;
    using difference_type =
        typename std::vector<
            T, typename allocator_declare<T, A>::type>::difference_type;

    static const size_type  value_size = sizeof(value_type);

    VectorView() = default;
    VectorView(const VectorView &) = default;
    VectorView(VectorView &&) = default;
    VectorView &operator= (const VectorView &) = default;
    VectorView &operator= (VectorView &&) = default;
    ~VectorView() = default;

    inline VectorView (value_type *bp, value_type *ep) noexcept
        : begin_ptr_(bp), end_ptr_(ep)  {   }

    // The purpose of this method is for the user be able to conform to STL
    // standards.
    // To create a VectorView over an entire std::vector you have to do this:
    //        VectorView(&(*v.begin()), &(*v.end()));
    // The above second parameter is against standards and it is caught
    // if you set the STL boundary check flag. So instead, you can do:
    //        VectorView vv;
    //        vv.set_begin_end_special(&(*v.begin()), &(*v.back()));
    //
    inline void set_begin_end_special(value_type *bp, value_type *ep_1)  {

        begin_ptr_ = bp;
        end_ptr_ = ep_1;
        end_ptr_ += 1;
    }

    [[nodiscard]] inline bool
    empty () const noexcept  { return (begin_ptr_ == end_ptr_); }
    [[nodiscard]] inline size_type size () const noexcept  {

        return (static_cast<size_type>(end_ptr_ - begin_ptr_));
    }
    [[nodiscard]] inline size_type
    capacity () const noexcept  { return (size()); }
    inline void clear () noexcept  { begin_ptr_ = end_ptr_ = nullptr; }

    [[nodiscard]] inline reference
    at (size_type i) noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline const_reference
    at (size_type i) const noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline reference
    operator [] (size_type i) noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline const_reference
    operator [] (size_type i) const noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline reference front() noexcept  { return (*begin_ptr_); }
    [[nodiscard]] inline const_reference
    front() const noexcept  { return (*begin_ptr_); }
    [[nodiscard]] inline reference
    back() noexcept  { return (*(end_ptr_ - 1)); }
    [[nodiscard]] inline const_reference
    back() const noexcept  { return (*(end_ptr_ - 1)); }

    // These are to match the std::vector interface, so things will compile
    //
    inline void shrink_to_fit()  {  }
    inline void reserve (size_type)  {  }

    inline void swap (VectorView &rhs) noexcept  {

        std::swap (begin_ptr_, rhs.begin_ptr_);
        std::swap (end_ptr_, rhs.end_ptr_);
        return;
    }

    VectorView &operator= (std::vector<T> &rhs)  {

        VectorView  vw(&*(rhs.begin()), &(rhs.back()) + 1);

        swap(vw);
        return (*this);
    }

public:

    class   iterator;
    class   const_iterator : public std::random_access_iterator_tag  {

    public:

        using iterator_category = std::random_access_iterator_tag;
        using value_type = T;
        using pointer = value_type *;
        using const_pointer = const value_type *;
        using reference = value_type &;
        using const_reference = const value_type &;
        using difference_type =
            typename std::vector<
                T, typename allocator_declare<T, A>::type>::difference_type;

    public:

        inline const_iterator (value_type const *const node) noexcept
            : node_ (node)  {   }
        inline const_iterator (const iterator &itr) noexcept
            : node_ (nullptr)  { *this = itr; }

        const_iterator () = default;
        ~const_iterator () = default;
        const_iterator (const const_iterator &) = default;
        const_iterator (const_iterator &&) = default;
        const_iterator &operator = (const const_iterator &) = default;
        const_iterator &operator = (const_iterator &&) = default;

        inline const_iterator &operator = (const iterator &rhs) noexcept  {

            node_ = rhs.node_;
            return (*this);
        }

        inline bool operator == (const const_iterator &rhs) const noexcept  {

            return (node_ == rhs.node_);
        }
        inline bool operator != (const const_iterator &rhs) const noexcept  {

            return (node_ != rhs.node_);
        }
        inline bool operator > (const const_iterator &rhs) const noexcept  {

            return (node_ > rhs.node_);
        }
        inline bool operator >= (const const_iterator &rhs) const noexcept  {

            return (node_ >= rhs.node_);
        }
        inline bool operator < (const const_iterator &rhs) const noexcept  {

            return (node_ < rhs.node_);
        }
        inline bool operator <= (const const_iterator &rhs) const noexcept  {

            return (node_ <= rhs.node_);
        }

       // Following STL style, this iterator appears as a pointer
       // to value_type.
       //
        inline const_reference operator * () const noexcept  {

            return (*node_);
        }
        inline const_pointer operator -> () noexcept  { return (node_); }

       // ++Prefix
       //
        inline const_iterator &operator ++ () noexcept  {

            node_ += 1;
            return (*this);
        }

       // Postfix++
       //
        inline const_iterator operator ++ (int) noexcept  {

            value_type   const  *ret_node = node_;

            node_ += 1;
            return (const_iterator (ret_node));
        }

        inline const_iterator &operator += (difference_type step) noexcept  {

            node_ += step;
            return (*this);
        }

       // --Prefix
       //
        inline const_iterator &operator -- () noexcept  {

            node_ -= 1;
            return (*this);
        }

       // Postfix--
       //
        inline const_iterator operator -- (int) noexcept  {

            value_type  const  *ret_node = node_;

            node_ -= 1;
            return (const_iterator (ret_node));
        }

        inline const_iterator &operator -= (difference_type step) noexcept  {

            node_ -= step;
            return (*this);
        }

        template<typename I>
        inline const_iterator operator + (I step) const noexcept  {

            value_type  const  *ret_node = node_;

            ret_node += static_cast<difference_type>(step);
            return (const_iterator (ret_node));
        }

        friend difference_type
        operator - (const_iterator lhs, const_iterator rhs) noexcept  {

            return (lhs.node_ - rhs.node_);
        }

        template<std::semiregular I>
        inline const_iterator operator - (I step) const noexcept  {

            value_type  const  *ret_node = node_;

            ret_node -= static_cast<difference_type>(step);
            return (const_iterator (ret_node));
        }

        inline const_reference &
        operator [](difference_type step) const noexcept  {

            return (*(node_ + step));
        }

    private:

        const_pointer   node_ { nullptr };
    };

   // This iterator contains only one pointer. Like STL iterators,
   // it is cheap to create and copy around.
   //
    class   iterator : public std::random_access_iterator_tag  {

    public:

        using iterator_category = std::random_access_iterator_tag;
        using value_type = T;
        using pointer = value_type *;
        using const_pointer = const value_type *;
        using reference = value_type &;
        using const_reference = const value_type &;
        using difference_type =
            typename std::vector<
                T, typename allocator_declare<T, A>::type>::difference_type;

    public:

        inline iterator (value_type *node) noexcept : node_ (node)  {  }

        iterator () = default;
        ~iterator () = default;
        iterator (const iterator &) = default;
        iterator (iterator &&) = default;
        iterator &operator = (const iterator &) = default;
        iterator &operator = (iterator &&) = default;

        inline bool operator == (const iterator &rhs) const noexcept  {

            return (node_ == rhs.node_);
        }
        inline bool operator != (const iterator &rhs) const noexcept  {

            return (node_ != rhs.node_);
        }
        inline bool operator > (const iterator &rhs) const noexcept  {

            return (node_ > rhs.node_);
        }
        inline bool operator >= (const iterator &rhs) const noexcept  {

            return (node_ >= rhs.node_);
        }
        inline bool operator < (const iterator &rhs) const noexcept  {

            return (node_ < rhs.node_);
        }
        inline bool operator <= (const iterator &rhs) const noexcept  {

            return (node_ <= rhs.node_);
        }

       // Following STL style, this iterator appears as a pointer
       // to value_type.
       //
        inline reference operator * () const noexcept { return (*node_); }
        inline pointer operator -> () noexcept { return (node_); }

       // We are following STL style iterator interface.
       //
        inline iterator &operator ++ () noexcept  {    // ++Prefix

            node_ += 1;
            return (*this);
        }
        inline iterator operator ++ (int) noexcept  {  // Postfix++

            value_type   *ret_node = node_;

            node_ += 1;
            return (iterator (ret_node));
        }

        inline iterator &operator += (difference_type step) noexcept  {

            node_ += step;
            return (*this);
        }

        inline iterator &operator -- () noexcept  {    // --Prefix

            node_ -= 1;
            return (*this);
        }
        inline iterator operator -- (int) noexcept  {  // Postfix--

            value_type   *ret_node = node_;

            node_ -= 1;
            return (iterator (ret_node));
        }

        inline iterator &operator -= (difference_type step) noexcept  {

            node_ -= step;
            return (*this);
        }

        template<typename I>
        inline iterator operator + (I step) const noexcept  {

            value_type  *ret_node = node_;

            ret_node += static_cast<difference_type>(step);
            return (iterator (ret_node));
        }

        friend difference_type
        operator - (iterator lhs, iterator rhs) noexcept  {

            return (lhs.node_ - rhs.node_);
        }

        template<std::semiregular I>
        inline iterator operator - (I step) const noexcept  {

            value_type  *ret_node = node_;

            ret_node -= static_cast<difference_type>(step);
            return (iterator (ret_node));
        }

        inline const_reference &
        operator [](difference_type step) const noexcept  {

            return (*(node_ + step));
        }
        inline reference &
        operator [](difference_type step) noexcept  {

            return (*(node_ + step));
        }

    private:

        pointer node_ { nullptr };

        friend class    VectorView::const_iterator;
    };

    friend class    iterator;
    friend class    const_iterator;

    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    [[nodiscard]] inline iterator
    begin () noexcept  { return (iterator (begin_ptr_)); }
    [[nodiscard]] inline iterator
    end () noexcept  { return (iterator (end_ptr_)); }
    [[nodiscard]] inline const_iterator
    begin () const noexcept  { return (const_iterator (begin_ptr_)); }
    [[nodiscard]] inline const_iterator
    end () const noexcept  { return (const_iterator (end_ptr_)); }

    [[nodiscard]] inline reverse_iterator
    rbegin() noexcept { return (std::make_reverse_iterator(end())); }
    [[nodiscard]] inline reverse_iterator
    rend() noexcept { return (std::make_reverse_iterator(begin())); }
    [[nodiscard]] inline const_reverse_iterator
    rbegin() const noexcept { return (std::make_reverse_iterator(end())); }
    [[nodiscard]] inline const_reverse_iterator
    rend() const noexcept { return (std::make_reverse_iterator(begin())); }

    [[nodiscard]] const_iterator
    cbegin() const { return (const_iterator (begin_ptr_)); }
    [[nodiscard]] const_iterator
    cend() const { return (const_iterator (end_ptr_)); }
    [[nodiscard]] const_reverse_iterator
    crbegin() const { return (std::make_reverse_iterator(end())); }
    [[nodiscard]] const_reverse_iterator
    crend() const { return (std::make_reverse_iterator(begin())); }

private:

    value_type  *begin_ptr_ { nullptr };
    value_type  *end_ptr_ { nullptr };
};

// ----------------------------------------------------------------------------

template<typename T, std::size_t A = 0>
class VectorConstView {

public:

    static constexpr std::align_val_t   align_value { A };

    using value_type = T;
    using size_type =
        typename std::vector<
            T, typename allocator_declare<T, A>::type>::size_type;
    using pointer = const value_type *;
    using const_pointer = const value_type *;
    using const_pointer_const = const value_type *const;
    using reference = const value_type &;
    using const_reference = const value_type &;
    using difference_type =
        typename std::vector<
            T, typename allocator_declare<T, A>::type>::difference_type;


    static const size_type  value_size = sizeof(value_type);

    VectorConstView() = default;
    VectorConstView(const VectorConstView &) = default;
    VectorConstView(VectorConstView &&) = default;
    VectorConstView &operator= (const VectorConstView &) = default;
    VectorConstView &operator= (VectorConstView &&) = default;
    ~VectorConstView() = default;

    inline
    VectorConstView (const value_type *bp, const value_type *ep) noexcept
        : begin_ptr_(bp), end_ptr_(ep)  {   }

    // The purpose of this method is for the user be able to conform to STL
    // standards.
    // To create a VectorView over an entire std::vector you have to do this:
    //        VectorView(&(*v.begin()), &(*v.end()));
    // The above second parameter is against standards and it is caught
    // if you set the STL boundary check flag. So instead, you can do:
    //        VectorView vv;
    //        vv.set_begin_end_special(&(*v.begin()), &(v.back()));
    //
    inline void
    set_begin_end_special(const value_type *bp, const value_type *ep_1)  {

        begin_ptr_ = bp;
        end_ptr_ = ep_1;
        end_ptr_ += 1;
    }

    [[nodiscard]] inline bool
    empty () const noexcept  { return (begin_ptr_ == end_ptr_); }
    [[nodiscard]] inline size_type size () const noexcept  {

        return (static_cast<size_type>(end_ptr_ - begin_ptr_));
    }
    inline void clear () noexcept  { begin_ptr_ = end_ptr_ = nullptr; }

    [[nodiscard]] inline const_reference
    at (size_type i) const noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline const_reference
    operator [] (size_type i) const noexcept  { return (*(begin_ptr_ + i)); }
    [[nodiscard]] inline const_reference
    ront() const noexcept  { return (*begin_ptr_); }
    [[nodiscard]] inline const_reference
    back() const noexcept  { return (*(end_ptr_ - 1)); }

    // These are to match the std::vector interface, so things will compile
    //
    inline void shrink_to_fit()  {  }
    inline void reserve (size_type)  {  }

    inline void swap (VectorConstView &rhs) noexcept  {

        std::swap (begin_ptr_, rhs.begin_ptr_);
        std::swap (end_ptr_, rhs.end_ptr_);
        return;
    }

    VectorConstView &operator= (const std::vector<T> &rhs)  {

        VectorConstView  vw(&*(rhs.begin()), &(rhs.back()) + 1);

        swap(vw);
        return (*this);
    }

public:

    class   const_iterator : public std::random_access_iterator_tag  {

    public:

        using iterator_category = std::random_access_iterator_tag;
        using value_type = T;
        using pointer = value_type *;
        using const_pointer = const value_type *;
        using reference = value_type &;
        using const_reference = const value_type &;
        using difference_type =
            typename std::vector<
                T, typename allocator_declare<T, A>::type>::difference_type;

    public:

        inline const_iterator (value_type const *const node) noexcept
            : node_ (node)  {   }

        const_iterator () = default;
        ~const_iterator () = default;
        const_iterator (const const_iterator &) = default;
        const_iterator (const_iterator &&) = default;
        const_iterator &operator = (const const_iterator &) = default;
        const_iterator &operator = (const_iterator &&) = default;

        inline bool operator == (const const_iterator &rhs) const noexcept  {

            return (node_ == rhs.node_);
        }
        inline bool operator != (const const_iterator &rhs) const noexcept  {

            return (node_ != rhs.node_);
        }
        inline bool operator > (const const_iterator &rhs) const noexcept  {

            return (node_ > rhs.node_);
        }
        inline bool operator >= (const const_iterator &rhs) const noexcept  {

            return (node_ >= rhs.node_);
        }
        inline bool operator < (const const_iterator &rhs) const noexcept  {

            return (node_ < rhs.node_);
        }
        inline bool operator <= (const const_iterator &rhs) const noexcept  {

            return (node_ <= rhs.node_);
        }

       // Following STL style, this iterator appears as a pointer
       // to value_type.
       //
        inline const_reference operator * () const noexcept  {

            return (*node_);
        }
        inline const_pointer operator -> () noexcept  { return (node_); }

       // ++Prefix
       //
        inline const_iterator &operator ++ () noexcept  {

            node_ += 1;
            return (*this);
        }

       // Postfix++
       //
        inline const_iterator operator ++ (int) noexcept  {

            value_type   const  *ret_node = node_;

            node_ += 1;
            return (const_iterator (ret_node));
        }

        inline const_iterator &operator += (difference_type step) noexcept  {

            node_ += step;
            return (*this);
        }

       // --Prefix
       //
        inline const_iterator &operator -- () noexcept  {

            node_ -= 1;
            return (*this);
        }

       // Postfix--
       //
        inline const_iterator operator -- (int) noexcept  {

            value_type  const  *ret_node = node_;

            node_ -= 1;
            return (const_iterator (ret_node));
        }

        inline const_iterator &operator -= (difference_type step) noexcept  {

            node_ -= step;
            return (*this);
        }

        template<typename I>
        inline const_iterator operator + (I step) const noexcept  {

            value_type  const  *ret_node = node_;

            ret_node += static_cast<difference_type>(step);
            return (const_iterator (ret_node));
        }

        friend difference_type
        operator - (const_iterator lhs, const_iterator rhs) noexcept  {

            return (lhs.node_ - rhs.node_);
        }

        template<std::semiregular I>
        inline const_iterator operator - (I step) const noexcept  {

            value_type  const  *ret_node = node_;

            ret_node -= static_cast<difference_type>(step);
            return (const_iterator (ret_node));
        }

        inline const_reference &
        operator [](difference_type step) const noexcept  {

            return (*(node_ + step));
        }

    private:

        const_pointer   node_ { nullptr };
    };

    using reverse_iterator = std::reverse_iterator<const_iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    friend class    const_iterator;

    [[nodiscard]] inline const_iterator
    begin () const noexcept  { return (const_iterator (begin_ptr_)); }
    [[nodiscard]] inline const_iterator
    end () const noexcept  { return (const_iterator (end_ptr_)); }

    [[nodiscard]] inline const_reverse_iterator
    rbegin() const noexcept { return (std::make_reverse_iterator(end())); }
    [[nodiscard]] inline const_reverse_iterator
    rend() const noexcept { return (std::make_reverse_iterator(begin())); }

    [[nodiscard]] const_iterator
    cbegin() const { return (const_iterator (begin_ptr_)); }
    [[nodiscard]] const_iterator
    cend() const { return (const_iterator (end_ptr_)); }
    [[nodiscard]] const_reverse_iterator
    crbegin() const { return (std::make_reverse_iterator(end())); }
    [[nodiscard]] const_reverse_iterator
    crend() const { return (std::make_reverse_iterator(begin())); }

private:

    const value_type    *begin_ptr_ { nullptr };
    const value_type    *end_ptr_ { nullptr };
};

} // namespace hmdf

// ----------------------------------------------------------------------------

// Local Variables:
// mode:C++
// tab-width:4
// c-basic-offset:4
// End:
