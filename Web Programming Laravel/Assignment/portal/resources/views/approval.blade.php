<!-- view where the Administrator can see currently unnapproved Restaurants and choose to approve them -->

@extends('layouts.app')

@section('content')
    <div class = "container">
        <div id = "content">
            <h1>Restaurants Requiring Approval: </h1>
            @if (count($restaurants) <= 0)
                <h4>No Restaurants Currently Require Approval</h4>
            @else
                <!-- display each Restaurant that requires approval -->
                @foreach ($restaurants as $restaurant)
                    <div id = "media">
                        <table>
                            <col width = "150">
                            <col width = "150">
                            <tr>
                                <th>Name: </th>
                                <th>Options: </th>
                            </tr>
                            <tr>
                                <td><h4>{{ $restaurant->name }}</h4></td>
                                <td>
                                    <!-- form for approving a new Restaurant -->
                                    <form method="Post" action="{{url("user/$restaurant->id")}}">
                                        {{ csrf_field() }}
                                        {{ method_field('PUT') }}
                                        <input type="submit" value="Approve">
                                    </form>
                                </td>
                            </tr>
                        </table>
                    </div>
                @endforeach
            @endif
            <div class = "pagination">
                {{ $restaurants->links() }}
            </div>
        </div>
    </div>
@endsection