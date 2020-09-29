<?php

use Illuminate\Database\Seeder;

class UsersTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */

    // function to seed Users into the Users table of the database
    public function run()
    {
        DB::table('users')->insert([
            'name' => 'Bob',
            'email' => 'Bob@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '22 Westbord Street, Greenslopes QLD 5143',
            'role' => 'Consumer',
            'approval' => false,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);

        DB::table('users')->insert([
            'name' => 'Greg',
            'email' => 'greg@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '19 Fake Street, Redcliffe QLD 5143',
            'role' => 'Consumer',
            'approval' => false,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);

        DB::table('users')->insert([
            'name' => 'McDonalds',
            'email' => 'mcdonalds@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '10 Peach Street, Westcity QLD 2143',
            'role' => 'Restaurant',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);

        DB::table('users')->insert([
            'name' => 'Grilled',
            'email' => 'grilled@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '712 Tyre Street, Varsity QLD 2143',
            'role' => 'Restaurant',
            'approval' => false,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);       

        DB::table('users')->insert([
            'name' => 'Wendys',
            'email' => 'wendys@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '10 Apple Street, Redcliffe QLD 2743',
            'role' => 'Restaurant',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);

        DB::table('users')->insert([
            'name' => 'Burger King',
            'email' => 'bkd@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '71 Fat Street, Varsity QLD 2143',
            'role' => 'Restaurant',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);  

        DB::table('users')->insert([
            'name' => 'Noodle Box',
            'email' => 'noodlebox@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '67 Noodle Street, Woodridge QLD 6113',
            'role' => 'Restaurant',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);

        DB::table('users')->insert([
            'name' => 'Subway',
            'email' => 'subway@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '87 Dunkin Street, Southbank QLD 4143',
            'role' => 'Restaurant',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);  

        DB::table('users')->insert([
            'name' => 'Admin',
            'email' => 'admin@gmail.com',
            'email_verified_at' => DB::raw('CURRENT_TIMESTAMP'),
            'address' => '87 Dunkin Street, Southbank QLD 4143',
            'role' => 'Administrator',
            'approval' => true,
            'password' => bcrypt('123456'),
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]);  
    }
}
