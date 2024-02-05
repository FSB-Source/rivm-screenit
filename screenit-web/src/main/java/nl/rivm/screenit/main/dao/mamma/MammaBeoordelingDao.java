package nl.rivm.screenit.main.dao.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

public interface MammaBeoordelingDao
{
	List<MammaBeoordeling> zoekBeBeoordelingen(MammaBeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc);

	List<MammaBeoordeling> zoekCeBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc);

	List<MammaScreeningsEenheid> screeningsEenhedenMetBeWerklijstBeoordeling(MammaBeWerklijstZoekObject zoekObject);

	List<MammaScreeningsEenheid> screeningsEenhedenMetCeWerklijstBeoordeling(MammaCeWerklijstZoekObject zoekObject);

	List<Long> zoekBeoordelingenNummers(MammaBeWerklijstZoekObject zoekObject, String sortProperty, boolean asc);

	long countCeWerklijstBeoordelingen(MammaCeWerklijstZoekObject zoekObject);

	long countBeWerklijstBeoordelingen(MammaBeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> getAlleVorigeBeoordelingenMetBeelden(MammaBeoordeling beoordeling);

	List<MammaBeoordeling> getVorigeBeoordelingen(MammaBeoordeling beoordeling, int aantal, boolean inclusiefGunstige);

	int getAantalBeoordeeldInList(List<Long> beoordelingenIds);

	int getAantalBeoordeeld(MammaBeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekCeWerklijstProcesmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countCeWerklijstProcesmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject);
}
