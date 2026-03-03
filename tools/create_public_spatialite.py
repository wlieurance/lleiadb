#!/usr/bin/env python3
"Creates empty LLEIA public/eco schema in sqlite/spatialite format."

import argparse
import sys
import sqlite3 as sqlite
import re
import pandas as pd
import os
import sqlparse
from datetime import datetime, date

# convert postgis to spatialite regex conversion
SL_REG = [
    {'p': r'\bTIMESTAMPZ?(?:\s*WITH(?:OUT)? TIME ZONE)?\b', 'r': r'DATETIME'},
    {'p': r'\bBYTEA\b', 'r': r'BLOB'},
    {'p': r'\bUUID\b', 'r': r'TEXT'},
    {'p': r'\bSERIAL\s+PRIMARY\s+KEY\b', 'r': r'INTEGER PRIMARY KEY AUTOINCREMENT'},
    {'p': 'CREATE OR REPLACE VIEW', 'r': 'CREATE VIEW IF NOT EXISTS'},
    {'p': r'(\s+REFERENCES\s+)("[A-Za-z\d_ ]+"|[A-Za-z\d_]+)\.', 'r': r'\1'},
    {'p': r"""POSITION\s*\(\s*('[^']+'|[A-Za-z_\."]+)\s+IN\s+('[^']+'|[A-Za-z_\."]+)\)""",
     'r': r'INSTR(\1, \2)'},
    {'p': r'SUBSTRING\(', 'r': r'SUBSTR('},
    {'p': r"""LEFT\s*\(\s*([A-Za-z"\.]+)\s*,\s*(\d+)\)""", 'r': r'SUBSTR(\1, 1, \2)'},
    {'p': r"""RIGHT\s*\(\s*([A-Za-z"\.]+)\s*,\s*(\d+)\)""", 'r': r'SUBSTR(\1, -1, \2)'},
    {'p': ''.join((
        r"""ALTER\s+TABLE\s+([A-Za-z{}"_]+)\.([A-Za-z{}"_]+)\s+ADD\s+COLUMN\s+(?:IF\s+NOT\s+""",
        r"""EXISTS\s+)?([A-Za-z{}"_]+)\s+([A-Za-z{}"_]+)\s*\('?([A-Za-z]+)'?\s*,\s*(\d{4})\);?"""
        )),
     'r': r"SELECT AddGeometryColumn('\2', '\3', \6, '\5', 2);"},
    {'p': ''.join((
        r'CREATE\s+INDEX\s+(?:IF\s+NOT\s+EXISTS\s+)?([A-Za-z"\{\}_]+)\s+ON\s+([A-Za-z"\{\}_]+)?',
        r'\.?([A-Za-z"\{\}_]+)\s+USING\s+GIST\s+\(([^\)]+)\);?'
        )),
     'r': r"SELECT CreateSpatialIndex('\3', '\4');"},
    {'p': r''.join((
        r'CREATE\s+INDEX\s+(IF\s+NOT\s+EXISTS\s+)?([A-Za-z"\{\}_]+)\s+ON\s+([A-Za-z"\{\}_]+)?',
        r'\.?([A-Za-z"\{\}_]+)\s+(?:USING\s+[A-Za-z\-]+\s+)?\(([^\)]+)\);?')),
     'r': r'CREATE INDEX \1 \2 ON \4 (\5)'},
    {'p': r'CREATE\s+SCHEMA\s+(?:IF\s+NOT\s+EXISTS\s+)?[^;]+;?', 'r': ''},
    {'p': r'DROP\s+(TABLE|VIEW)(\s+IF\s+EXISTS)?\s+(.+?)\s+CASCADE;?', 'r': r'DROP \1\2 \3;'}
  # {'p': r'', 'r': r''}
    ]



def dict_factory(cursor, row):
    "For dictionary row_factory in sqlite3"
    fields = [column[0] for column in cursor.description]
    return dict(zip(fields, row))

def connect_db(dbpath, drop=False, spatialite = True):
    exists = os.path.isfile(dbpath)
    if exists and drop:
        os.remove(dbpath)
    con = sqlite.connect(dbpath)
    con.enable_load_extension(True)
    con.execute("SELECT load_extension('mod_spatialite');")
    if (not exists or drop) and spatialite:
        con.execute("SELECT InitSpatialMetaData(1);")
    con.row_factory = dict_factory
    con.execute("PRAGMA foreign_keys = ON;")
    con.commit()
    return con


def convert_geom(stmts):
    geom_pat = r'([A-Za-z\d_"]+)\s+(geometry|geography)\(([A-Z]+)\s*,\s*(\d+)\)\s*,?'
    conv_stmts = []
    geom_add_idx = []
    geom_drop_stmts = [] 
    for s in stmts:
        geom_search = re.search(geom_pat, s, flags=re.I)
        tbl_search = re.search('CREATE\s+TABLE\s+(?:([^\.]+)\.)?([A-Za-z\d_"]+)', s, 
                               flags=re.I)
        if tbl_search and geom_search:
            schema_name = tbl_search.group(1)
            table_name = tbl_search.group(2)
            col_name = geom_search.group(1)
            col_type = geom_search.group(2)
            geom_type = geom_search.group(3)
            srid = geom_search.group(4)
            frmt_stmt = re.sub(geom_pat, '', s, flags=re.I)
            fix_stmt = re.sub(',(\s*\)[\s;]*)$', r'\1', frmt_stmt)  # fixes orphan commas
            geom_add = ''.join((
                f"SELECT AddGeometryColumn('{table_name}', '{col_name}', {srid}, ",
                f"'{geom_type}');"
            ))
            conv_stmts.append(fix_stmt)
            conv_stmts.append(geom_add)
            geom_add_idx.append(len(conv_stmts) - 1)
            geom_drop_stmts.append({'tab': table_name, 'col': col_name})
        else:
            conv_stmts.append(s)
    return conv_stmts, geom_add_idx, geom_drop_stmts      

def convert_statements(stmts, params={}):
    rep_stmts = stmts.copy()  # iterative starting block
    for sr in SL_REG:
        pattern = re.compile(sr['p'], re.I)
        rep_stmts = [re.sub(pattern, sr['r'], x) for x in rep_stmts]
    frmt_stmts = [x.format(**params) for x in rep_stmts if x.strip()]
    geom_stmts, add_idx, drop_stmts = convert_geom(frmt_stmts)
    return geom_stmts, add_idx, drop_stmts

def try_execute(con, stmt_list):
    cur = con.cursor()
    for i, stmt in enumerate(stmt_list):
        try:
            cur.execute(stmt)
        except sqlite.OperationalError as e:
            print(f"Error in statement {i}")
            print(stmt)
            con.rollback()
            raise
    con.commit()

def drop_geom_cols(con, drop_list):
    dropcheck_sql = '\n'.join((
        "SELECT 1 FROM geometry_columns WHERE f_table_name = ? AND f_geometry_column = ?;",
    ))
    cur = con.cursor()
    for d in drop_list:
        cur.execute(dropcheck_sql, (d['tab'], d['col']))
        if cur.fetchone():
            cur.execute("SELECT DiscardGeometryColumn(?, ?);", (d['tab'], d['col']))


def execute_sql_file(con, sql_path, strip_schema=None, convert=False, drop_geom=False, params = {}):
    with open(sql_path, 'r') as file:
        raw = file.read()
    if strip_schema:
        noschema = re.sub(rf"\s+{strip_schema}\.", r" main.", eco_raw, flags=re.I)
    else:
        noschema = raw
    stmts = sqlparse.split(noschema)
    if convert:
        frmt, add_idx, drop_list = convert_statements(stmts, params)
        if drop_geom:  
            drop_geom_cols(con, drop_list)
    else:
        frmt = stmts
    try_execute(con, frmt)


def create_eco(eco_db, lleia_sql, spatial_type, drop_eco):
    eco_schema = os.path.join(lleia_sql, 'create_eco_tables.sql')
    public_schema = os.path.join(lleia_sql, 'create_public_tables.sql')
    public_data = os.path.join(lleia_sql, 'create_public_data.sql')
    sqlite_views = os.path.join(lleia_sql, 'create_sqlite_views.sql')
    if spatial_type == 'esri':
        poly_dir = 'ST_ForcePolygonCW'
    else:
        poly_dir = 'ST_ForcePolygonCCW'

    eco_con = connect_db(eco_db, drop=drop_eco) 
    
    # eco create tables
    print("Creating eco tables...")
    execute_sql_file(con=eco_con, sql_path=eco_schema, strip_schema="eco", convert=True,
                     drop_geom=True)
    
    # public create tables
    print("Creating shared tables...")
    execute_sql_file(con=eco_con, sql_path=public_schema, strip_schema="public", convert=True,
                     drop_geom=True)

    # public insert data
    print("Inserting shared data...")
    execute_sql_file(con=eco_con, sql_path=public_data, strip_schema="public", convert=False)
    
    # # sqlite file needs conversion work
    # print("Creating views...")
    # execute_sql_file(con=eco_con, sql_path=sqlite_views, strip_schema="public", convert=True,
    #                  drop_geom=True)

    # params={'st_direction': poly_dir}
    eco_con.close()


def main(eco_db, lleia_sql, spatial_type, drop_eco):
    create_eco(eco_db, lleia_sql, spatial_type, drop_eco)



if __name__ == '__main__':
    argv = sys.argv[1:]
    parser = argparse.ArgumentParser(
        prog='ConvertLLEIAsqlite',
        description='Creates a blank spatialite version of the LLEIA public schema.')
    parser.add_argument('eco_db',
                        help="a file path to which to write conversion results to (spatialite).")
    parser.add_argument('lleia_sql',
                        help="the path to the LLEIADB sql directory.")
    parser.add_argument('-d', '--drop_eco', action='store_true',
                        help="re-create eco schema tables from scratch")
    parser.add_argument('-t', '--spatial_type', choices=['sf', 'esri'], default='sf',
                        help='Polygon construction method. "sf" will use the ISO 19125-1 OGC '
                             'Simple Features standard (CCW), and "esri" will use the ESRI/Arc '
                             'standard (CW).')
    args = parser.parse_args(argv)

    main(eco_db=args.eco_db, lleia_sql=args.lleia_sql,
         spatial_type=args.spatial_type, drop_eco=args.drop_eco)
    print('Script finished.')
