package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;

public interface BezwaarService
{

	List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment);

	List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment, boolean excludeDossierBezwaar);

	List<BezwaarGroupViewWrapper> getBezwaarGroupViewWrappers(BezwaarMoment moment, boolean verzoekTotBezwaarTeZien);

	BezwaarBrief maakBezwaarAanvraag(Client client, boolean zonderHandtekening, BriefType briefType);

	BezwaarBrief maakBezwaarAanvraag(Client client);

	void bezwaarAfronden(BezwaarMoment moment, Account account, List<BezwaarGroupViewWrapper> groupWrappers) throws IllegalStateException;

	boolean isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(BezwaarMoment bezwaarMoment, BezwaarType bezwaarType);

	BezwaarGroupViewWrapper getGroupWrapperForClientPortaal(BezwaarMoment laatstVoltooideMoment, Bevolkingsonderzoek bevolkingsonderzoek);

	void bezwarenDoorvoeren(BezwaarMoment moment);

	void bezwaarBRPIntrekken(Account account, Client client, UploadDocument document) throws IOException;

	boolean bezwaarDocumentenVervangen(UploadDocument nieuwDocument, BezwaarMoment bezwaarMoment, UploadDocument huidigDocument, Account account);

	boolean bezwarenGewijzigd(BezwaarMoment laatsteVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> wrappers, Bevolkingsonderzoek bvo);

	boolean bezwarenGewijzigd(BezwaarMoment laatsteVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> wrappers);

	boolean checkBezwaarInLaatsteBezwaarMomentAanwezigIs(Client client, BezwaarType bezwaarType);

	boolean heeftBezwaarIngediendInAfgelopenAantalDagen(Client client, BezwaarType bezwaarType, Bevolkingsonderzoek bevolkingsonderzoek, int aantalDagen);

	void bezwaarAfrondenVanuitClientPortaal(Client client, List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers);

	boolean isErEenBezwaarMetType(List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers, BezwaarType type);

	Optional<BezwaarBrief> getLaatsteBezwaarBriefVanTypeVoorClient(Client client, BriefType briefType);

	List<BezwaarBrief> getBezwaarBrievenVanClient(Client client);
}
