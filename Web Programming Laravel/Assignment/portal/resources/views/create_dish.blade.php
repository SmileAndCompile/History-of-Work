<!-- view where a Restaurant (User) can create a new dish -->

@extends('layouts.app')

@section('title')
    Create Dish
@endsection

@section('content')
<div class = "container">
    <div id="content">
        <div id="media">
            <!-- form for creating a new dish (can only create dishes if you are a Restaurant, and only for your own Restaurant) -->
            <form method="Post" action="{{url("dish/")}}" enctype="multipart/form-data">
                {{ csrf_field() }}
                <table>
                    <col width="100">
                    <col width="100">
                    <col width="100">
                    <col width="100">
                    <tr>
                        <th><label>Name: </label></th>
                        <th><label>Price: </label></th>
                        <th><label>Image: </label></th>
                        <th><label>Submit: </label></th>
                    </tr>
                    <tr>
                        <td height = "60">
                            <!-- retrieve old values if errors are found, otherw -->
                            @if(count($errors) > 0)
                                <input type="text" name="name" value="{{old('name')}}">
                            @else
                                <input type="text" name="name">
                            @endif
                        </td>
                        <td height = "60">
                            @if(count($errors) > 0)
                                <input type="text" name="price" value="{{old('price')}}">
                            @else
                                <input type="text" name="price">
                            @endif
                        </td height = "60">
                        <td height = "60">
                            @if(count($errors) > 0)
                                <input type="file" name="image" value="{{old('image')}}">
                            @else
                                <input type="file" name="image">
                            @endif
                        </td height = "60">
                        <td>
                            <input type="submit" value="Create">
                        </td>
                    </tr>
                    <tr>
                        <td height = "60">
                            <!-- display errors below the relevant input field if any errors are found in the input -->
                            @if($errors->first('name'))
                                <div class = "alert">
                                    {{$errors->first('name')}}
                                </div>
                            @endif
                        </td height = "60">
                        <td>
                            @if($errors->first('price'))
                                <div class = "alert">
                                    {{$errors->first('price')}}
                                </div>
                            @endif
                        </td>
                        <td height = "60">
                            @if($errors->first('image'))
                                <div class = "alert">
                                    {{$errors->first('image')}}
                                </div>
                            @endif
                        </td>
                        <td height = "60"></td>
                    </tr>
                </table>
            </form>
        </div>
    </div>
</div>
        
@endsection('content') 