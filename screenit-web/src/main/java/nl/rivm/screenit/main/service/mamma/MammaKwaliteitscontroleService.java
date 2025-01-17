package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaIKwaliteitscontrole;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;

public interface MammaKwaliteitscontroleService
{
	List<String> saveOrUpdateFotobespreking(MammaFotobespreking fotobespreking, File file);

	String addFotobesprekingOnderzoek(MammaFotobespreking fotobespreking, Client client);

	void wijzigOnderzoekStatus(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek, MammaFotobesprekingOnderzoekStatus nieuweStatus);

	void wijzigOnderzoekStatus(MammaVisitatieOnderzoek visitatie, MammaVisitatieOnderzoekStatus nieuweOnderzoekStatus);

	void startKwaliteitscontrole(MammaIKwaliteitscontrole kwaliteitscontrole);

	void fotobesprekingAfronden(MammaFotobespreking fotobespreking);

	void herbeoordeelFotobesprekingOnderzoek(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek);

	List<String> saveOrUpdateVisitatie(MammaVisitatie visitatie, Map<MammaVisitatieOnderdeel, File> fileMap, Map<File, String> fileNameMap, UploadDocument rapportageBijlage,
		UploadDocument vragenlijstBijlage);

	void kwaliteitscontroleAfronden(MammaIKwaliteitscontrole kwaliteitscontrole);

	void visitatieAfronden(MammaVisitatie visitatie);

	void wijzigOnderzoekStatus(MammaAdhocMeekijkverzoek onderzoek, MammaVisitatieOnderzoekStatus nieuweOnderzoekStatus);

	void deleteBesprokenFotobesprekingOnderzoek(MammaFotobesprekingOnderzoek fotobesprekingOnderzoek);

	void deleteFotobespreking(MammaFotobespreking fotobespreking);

	void deleteVisitatieOnderzoek(MammaVisitatieOnderzoek visitatieOnderzoek);

	void deleteVisitatie(MammaVisitatie visitatie);

	String addVisitatieOnderzoek(MammaVisitatie visitatie, MammaVisitatieOnderdeel visitatieOnderdeel, Client client);
}
