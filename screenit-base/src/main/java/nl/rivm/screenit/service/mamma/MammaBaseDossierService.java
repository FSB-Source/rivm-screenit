package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.stream.Stream;

import nl.rivm.screenit.exceptions.MammaStandplaatsVanPostcodeOnbekendException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;

public interface MammaBaseDossierService
{
	MammaFactorType getFactorType(MammaDossier dossier);

	long aantalOproepen(MammaDossier dossier);

	long aantalOpgekomenSE(MammaDossier dossier);

	long aantalOpgekomenBE(MammaDossier dossier);

	Stream<MammaScreeningRonde> laatste3AfgerondeRondesMetOnderzoek(MammaDossier dossier);

	boolean isAfspraakMakenMogelijk(MammaDossier dossier, boolean viaClientportaal, boolean viaSe);

	boolean isVerzettenMogelijk(MammaDossier dossier);

	MammaOnderzoek getLaatsteOnderzoek(MammaDossier dossier);

	boolean isAfspraakForcerenMogelijk(MammaDossier dossier);

	boolean isRondeForcerenMogelijk(MammaDossier dossier);

	void rondeForceren(Client client) throws MammaStandplaatsVanPostcodeOnbekendException;

	boolean isAutomatischRondeForcerenNaHeraanmeldenMogelijk(MammaDossier dossier);

	void maakDossierLeeg(MammaDossier dossier);

	boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, InstellingGebruiker medewerker, DashboardStatus dashboardStatus);

}
