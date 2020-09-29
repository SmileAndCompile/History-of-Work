@extends('layouts.app')

@section('title')
    Edit Dish
@endsection

@section('content')
<div class = "container">
    <div id="content">
        <div id="media">

            <!-- form for editing an existing dish (can only be accessed by Dish Owner) -->
            <form method="Post" action="{{url("dish/$dish->id")}}" enctype="multipart/form-data">
                {{ csrf_field() }}
                {{method_field('PUT')}}
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
                            <!-- if any errors are found, display old values - otherwise display existing values of Dish -->
                            @if(count($errors) > 0)
                                <input type="text" name="name" value="{{old('name')}}">
                            @else
                                <input type="text" name="name" value="{{$dish->name}}">
                            @endif
                        </td>
                        <td height = "60">
                            @if(count($errors) > 0)
                                <input type="text" name="price" value="{{old('price')}}">
                            @else
                                <input type="text" name="price" value="{{$dish->price}}">
                            @endif
                        </td height = "60">
                        <td height = "60">
                            @if(count($errors) > 0)
                                <input type="file" name="image" value="{{old('image')}}">
                            @else
                                <input type="file" name="image" value="{{$dish->image}}">
                            @endif
                        </td height = "60">
                        <td>
                            <input type="submit" value="Update">
                        </td>
                    </tr>
                    <tr>
                        <td height = "60">
                            <!-- display any errors found in the input -->
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