def add_to_library(library, artist, song, length):
    '''Adds Artist as a key, along with an empty dictionary as key value, which will get updated with songs and corresponding length as key and value repsepctively.'''
    if artist not in library:
        library[artist] = {}
    else:
        library[artist].update({song : length})
    library[artist].update({song : length})

def string_to_seconds(length):
    '''Converts a string of the form ##:## into the seconds equivalent.'''
    length_divided = length.split(':')
    if len(length_divided) != 2:
        raise ValueError()      
    try:
        return(int(length_divided[0])*60 + int(length_divided[1]))
    except: 
        raise ValueError()

def read_library(filename):
    '''Returns a dictionary version of the .txt file, as long as each entry is comma separated and the last entry of each row in the file is a string following the '##:##'-format.'''
    library = {}
    try: 
        with open(filename, 'r') as h_in:
            for n in h_in:
                artist, song, length = n.split(',')
                length = string_to_seconds(length) #This is where we raise ValueError if the format is incorrect.
                add_to_library(library, artist, song, length)
    except ValueError:
        raise ValueError('We do not support this file, try another.')
    except FileNotFoundError:
        raise FileNotFoundError('That file does not exist.')
    return(library)

def seconds_to_string(seconds):
    '''self-explanatory'''
    return f"{seconds // 60}:{seconds % 60:02d}"

def sum_song_lengths(library, artist):
    '''returns the amount of seconds all songs of an artist make up, as well as how many songs'''
    list_of_song_length = [] 
    for song in library[artist]:
        list_of_song_length.append(library[artist][song])
    return(sum(list_of_song_length), len(list_of_song_length))

def print_artist_songs(library, artist):
    '''Prints the songs that are inside the library which is held as the value of the artist in library.'''
    for song in library[artist]:
        print('-', song, '(' + seconds_to_string(library[artist][song]) + ')')

def print_library(library):
    '''Prints out the library in "read_library()" in an organised way.'''
    for artist in library:
        total_length, song_count = sum_song_lengths(library, artist)
        length_in_nice_form = seconds_to_string(total_length)
        print(artist, '(' + str(song_count), 'songs,', length_in_nice_form + ')')
        print_artist_songs(library, artist)

def make_playlist(library, theme):
    '''Returns a list of tuples (artist, song, length) based on if the theme-string is in the song-string.'''
    playlist = []
    for artist in library:
        for song in library[artist]:
            if theme in song:
                playlist.append((artist, song, int(library[artist][song])))
    if playlist==[]:
        raise ValueError('No songs match this theme')
    return playlist

def write_playlist(playlist, filename):
    '''Creates a .txt file out of the list returned by make_playlist.'''
    out_file = open(filename, 'w')
    for item in playlist:
        artist, song, length = item[0], item[1], seconds_to_string(item[2])
        out_file.write(artist + ',' + ' ' + song + ',' + ' ' + length + '\n')
    out_file.close()

def ask_for_playlist(library):
    '''A "middle-man" function for making make_playlist() interactive.'''
    theme = input('Enter a playlist theme:\n')
    return(make_playlist(library, theme))

def main():
    while True:
        try:
            filename_in = input('Which music library do you want to load?\n')
            library = read_library(filename_in)
            print_library(library)
            while True:
                try:    
                    playlist = ask_for_playlist(library)
                    break
                except ValueError:
                    pass  
            print(playlist)
            filename_out = input('Where do you want to save the playlist?\n')
            write_playlist(playlist,filename_out)
            print('Saved. Goodbye!')
            break
        except:
            pass
main()
