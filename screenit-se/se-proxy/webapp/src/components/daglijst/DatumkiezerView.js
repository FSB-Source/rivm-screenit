/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React from 'react';
import DatePicker, {registerLocale} from 'react-datepicker';
import moment from 'moment';
import 'react-datepicker/dist/react-datepicker.css';
import nl from 'date-fns/locale/nl';

registerLocale('nl', nl);

declare var Intl: any;

moment.updateLocale('nl', {
    months: ['januari', 'februari', 'maart', 'april', 'mei', 'juni', 'juli', 'augustus', 'september', 'oktober', 'november', 'december'],
    weekdaysMin: ['zo', 'ma', 'di', 'wo', 'do', 'vr', 'za'],
    week: {dow: 1},
});

type DatumkiezerProps = {
    daglijstDatum: string;
    online: boolean;
    onChooseDate: (datum: string, online: boolean) => mixed;
}

const DatumkiezerView = (props: DatumkiezerProps) => {
    return (
        <div className='row datumkiezer-div'>
            <div className='col-10 row-no-gutters'>
                <DatePicker
                    className="datumkiezer-view clickable"
                    locale="nl"
                    value={getDatumkiezerValue(props)}
                    onChange={(newDate) => {
                        props.onChooseDate(moment(newDate).format('YYYY-MM-DD'), props.online);
                    }}>
                </DatePicker>
            </div>
            <div className='col-2 datumkiezer-icon'>
                <i className="fa fa-calendar px-1 py-1"/>
            </div>
        </div>
    );
};

const getDatumkiezerValue = (props: any) => {
    const dateFormatOptions = {weekday: 'long', year: 'numeric', month: '2-digit', day: '2-digit'};
    return new Intl.DateTimeFormat('nl-NL', dateFormatOptions).format(moment(props.daglijstDatum));
};

export default DatumkiezerView;
