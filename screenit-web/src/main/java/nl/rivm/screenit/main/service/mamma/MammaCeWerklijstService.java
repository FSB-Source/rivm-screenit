package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

public interface MammaCeWerklijstService
{
	long countOnderzoeken(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekOnderzoeken(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	List<MammaOnderzoek> zoekOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String property, boolean ascending);

	long countProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> zoekFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject);
}
