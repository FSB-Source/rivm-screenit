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

import {combineReducers} from 'redux';
import afsprakenById from './AfspraakReducer';
import daglijstDatum from './DaglijstDatumReducer';
import dagverslag from './DagverslagReducer';
import nietAfgeslotenVanaf from './NietAfgeslotenVanafReducer';
import seGebruikers from './SeGebruikersReducer';
import session from './SessionReducer';
import clientenById from './ClientReducer';
import navigation from './NavigationReducer';
import type {State} from '../datatypes/State';
import planning from './PlanningReducer';
import visueleInspectieAfbeeldingByAfspraakId from './VisueleInspectieReducer';
import environmentInfo from './EnvironmentInfoReducer';
import autorisatie from './AutorisatieReducer';
import onderzoekByAfspraakId from './OnderzoekReducer';
import signaleringByAfspraakId from './SignalerenReducer';
import error from './ErrorReducer';
import online from './ConnectionReducer';
import formsByFormId from './FormReducer';
import huisartsenById from './HuisartsenReducer';
import mammografenById from './MammografenReducer';
import huidigeMammograafId from './MammograafReducer';
import zorginstellingen from './ZorginstellingenReducer';
import popup from './PopupReducer';
import pendingUpdates from './UpdateReducer';
import heeftWijzigingen from './WijzigingenReducer';
import bezigMetKwaliteitsopnameVolgnr from './BezigMetKwaliteitsopnameVolgnrReducer';
import activeStudyForIms from './ImsReducer';
import dubbeleInstantie from './DubbeleInstantieReducer';
import loginStatus from './LoginStatusReducer';
import opgehaaldeDagen from './OpgehaaldeDagenReducer';
import connectieStatus from './ConnectieStatusReducer';
import mammografenStatus from './MammografenStatusReducer';

const seReducers: State = combineReducers({
    afsprakenById,                              
    daglijstDatum,                              
    dagverslag,                                 
    nietAfgeslotenVanaf,                        
    seGebruikers,                               
    clientenById,                               
    planning,                                   
    session,                                    
    navigation,                                 
    visueleInspectieAfbeeldingByAfspraakId,     
    formsByFormId,                              
    environmentInfo,                            
    autorisatie,                                
    onderzoekByAfspraakId,                      
    signaleringByAfspraakId,                    
    huisartsenById,                             
    zorginstellingen,                           
    error,                                      
    pendingUpdates,                             
    mammografenById,                            
    huidigeMammograafId,                        
    popup,                                      
    heeftWijzigingen,                           
    online,                                     
    activeStudyForIms,                          
    bezigMetKwaliteitsopnameVolgnr,             
    dubbeleInstantie,                           
    loginStatus,                                
    opgehaaldeDagen,                            
    connectieStatus,                            
    mammografenStatus,                          
});

export default seReducers;
