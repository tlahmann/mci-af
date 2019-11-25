# -*- coding: UTF-8 -*-
import os
# importing csv module 
import csv
# Pygame-Modul importieren.
import pygame
import datetime
 
# Überprüfen, ob die optionalen Text- und Sound-Module geladen werden konnten.
if not pygame.font: print('Fehler pygame.font Modul konnte nicht geladen werden!')
if not pygame.mixer: print('Fehler pygame.mixer Modul konnte nicht geladen werden!')
 

rootdir = ".\\_StudyResults"
extension = 'csv'

headers = []
rows = []
WHITE =     (255, 255, 255)
BLACK =     (  0,   0,   0)
BLUE =      (  0,  20, 250, 10)
GREEN =     (  0, 255,   0)
COLORS = [
    (158,1,66, 100),
    (213,62,79, 100),
    (244,109,67, 100),
    (253,174,97, 100),
    (254,224,139, 100),
    (255,255,191, 100),
    (230,245,152, 100),
    (171,221,164, 100),
    (102,194,165, 100),
    (50,136,189, 100),
    (94,79,162, 100)
]

canvas_size = (1000, 1000)

def getMovement(partId):
    for subdir, dirs, files in os.walk(rootdir):
        if partId not in subdir: 
            continue
        for file in files:
            if file.endswith(extension):
                if file.endswith('headset.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        time = [e[0].strip() for e in lt]
                        x_pos = [float(e[1].strip().replace(',','.')) for e in lt]
                        y_pos = [float(e[2].strip().replace(',','.')) for e in lt]
                        z_pos = [float(e[3].strip().replace(',','.')) for e in lt]
                        print("\"\"\"\"####;;;;", partId, ";;;;####\"\"\"\"")
                        print("number of samples: ", len(time), "(", int(time[-1]) - int(time[0]), "ticks)")
                        print("time: ", convert_dotnet_tick(int(time[0])) , "to", convert_dotnet_tick(int(time[-1])) )
                        print("\n Average HMD position:\n")
                        avgs = (sum(x_pos) / len(x_pos), sum(y_pos) / len(y_pos), sum(z_pos) / len(z_pos))
                        print('\tX: ', avgs[0])
                        print('\tY: ', avgs[1])
                        print('\tZ: ', avgs[2])
                        x_pos = [x-avgs[0] for x in x_pos]
                        y_pos = [y-avgs[1] for y in y_pos]
                        z_pos = [z-avgs[2] for z in z_pos]
                        print("moved to center")
                        # x_norm = [float(x)/max(x_pos) for x in x_pos]
                        # y_norm = [float(y)/max(y_pos) for y in y_pos]
                        # z_norm = [float(z)/max(z_pos) for z in z_pos]
                        print(" Maximum offset from center:\n")
                        print('\tX: ', max(map(abs, x_pos)))
                        print('\tY: ', max(map(abs, y_pos)))
                        print('\tZ: ', max(map(abs, z_pos)))
                        print()
                        p = (partId, time, x_pos, y_pos, z_pos)
                        return p

def convert_dotnet_tick(ticks):
    """Convert .NET ticks to formatted ISO8601 time
    Args:
        ticks: integer
            i.e 100 nanosecond increments since 1/1/1 AD"""
    _date = datetime.datetime(1, 1, 1) + \
        datetime.timedelta(microseconds=ticks // 10)
    if _date.year < 1900:  # strftime() requires year >= 1900
        _date = _date.replace(year=_date.year + 1900)
    return _date.strftime("%Y-%m-%dT%H:%M:%S.%fZ")[:-3]

def main(participants):
    # Initialisieren aller Pygame-Module und    
    # Fenster erstellen (wir bekommen eine Surface, die den Bildschirm repräsentiert).
    pygame.init()
    screen = pygame.display.set_mode(canvas_size)
    # print(movement)
 
    # Titel des Fensters setzen, Mauszeiger nicht verstecken und Tastendrücke wiederholt senden.
    pygame.display.set_caption("Pygame-Tutorial: Grundlagen")
    pygame.mouse.set_visible(1)
    pygame.key.set_repeat(1, 30)
 
    # Clock-Objekt erstellen, das wir benötigen, um die Framerate zu begrenzen.
    clock = pygame.time.Clock()
 
    # create a font object. 
    # 1st parameter is the font file 
    # which is present in pygame. 
    # 2nd parameter is size of the font 
    font = pygame.font.Font('freesansbold.ttf', 32) 

    # create a text suface object, 
    # on which text is drawn on it. 
    # text = font.render(participant[0], True, GREEN, BLACK) 

    # # create a rectangular object for the 
    # # text surface object 
    # textRect = text.get_rect()  

    # # set the center of the rectangular object. 
    # textRect.center = (70, 50)
    
    # screen-Surface mit Schwarz (RGB = 0, 0, 0) füllen.
    

    # Die Schleife, und damit unser Spiel, läuft solange running == True.
    running = True
    while running:
        # Framerate auf 30 Frames pro Sekunde beschränken.
        # Pygame wartet, falls das Programm schneller läuft.
        clock.tick(60)
        screen.fill(BLACK)

        # copying the text surface object 
        # to the display surface object  
        # at the center coordinate. 
        # screen.blit(text, textRect)

        for idx, participant in enumerate(participants):
            if participant[2]:
                tX = participant[2].pop(0)*100
                tZ = participant[4].pop(0)*100
            else:
                tX = 0
                tZ = 0
            # print(tX, tZ)
            posX = int(canvas_size[0]/2 + tX)
            posZ = int(canvas_size[1]/2 + tZ)

            surface1 = pygame.Surface((10,10), pygame.SRCALPHA)
            # surface1.set_colorkey((0,0,0))
            # surface1.set_alpha(10)
            # pygame.draw.circle(surface1, BLUE, (posX, posZ), 50)
            COLOR = COLORS[idx % len(COLORS)]
            pygame.draw.rect(surface1, COLOR, surface1.get_rect(), 10)
            screen.blit(surface1, (posX, posZ))

            # t = convert_dotnet_tick(int(participant[1].pop(0)))
            # screen.blit(font.render(t, True, GREEN, BLACK), (posX + 50, posZ + 50))
 
        # Alle aufgelaufenen Events holen und abarbeiten.
        for event in pygame.event.get():
            # Spiel beenden, wenn wir ein QUIT-Event finden.
            if event.type == pygame.QUIT:
                running = False
 
            # Wir interessieren uns auch für "Taste gedrückt"-Events.
            if event.type == pygame.KEYDOWN:
                # Wenn Escape gedrückt wird, posten wir ein QUIT-Event in Pygames Event-Warteschlange.
                if event.key == pygame.K_ESCAPE:
                    pygame.event.post(pygame.event.Event(pygame.QUIT))
 
        # Inhalt von screen anzeigen.
        pygame.display.flip()
 
 
# Überprüfen, ob dieses Modul als Programm läuft und nicht in einem anderen Modul importiert wird.
if __name__ == '__main__':
    mvmnt = []

    with open(os.path.join(rootdir, 'resync_clean_combined.csv'), 'r') as csvfile:
        # creating a csv reader object 
        csvreader = csv.DictReader(csvfile, delimiter=';')
        headers = [e.lstrip() for e in csvreader.fieldnames]
        print(headers)
        pid = ''
        for row in csvreader:
            pid = row['Participant_ID']
            mvmnt.append(getMovement(pid))
    # Unsere Main-Funktion aufrufen.
    main(mvmnt)
