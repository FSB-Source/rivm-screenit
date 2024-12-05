package nl.rivm.screenit.service.mamma;

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

import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.util.KeyValue;

import org.springframework.data.jpa.domain.Specification;

import ca.uhn.hl7v2.HL7Exception;

public interface MammaBaseOnderzoekService
{
	void onderzoekDoorvoerenVanuitSe(MammaOnderzoek onderzoek);

	MammaBeoordeling voegInitieleBeoordelingToe(MammaOnderzoek onderzoek);

	void ontvangBeeldenVoorOnderzoek(Client client, MammaScreeningRonde ronde, MammaOnderzoekType onderzoekType) throws HL7Exception;

	void beeldenVerwijderdVoorOnderzoek(MammaIMSBericht bericht, Client client, boolean error);

	boolean isOnderzoekOnvolledigZonderFotos(MammaOnderzoek onderzoek);

	boolean isOnderzoekOnvolledigMetFotos(MammaOnderzoek onderzoek);

	List<KeyValue> vorigeRondeTeksten(MammaOnderzoek onderzoek, boolean opSE);

	void vervolgOnderbrokenOnderzoeken(MammaOnderzoek onderzoek);

	void voegNieuweBeoordelingToe(MammaOnderzoek onderzoek);

	void setMammografieStatus(MammaMammografie mammografie, MammaMammografieIlmStatus status);

	List<MammaOnderzoek> getOnderzoekenMetBeelden(Client client);

	boolean forceerMammografieIlmStatus(long accessionNumber, MammaMammografieIlmStatus status, Account account);

	boolean heeftBinnenMammografieIntervalGeenOnderzoekGehad(MammaDossier dossier);

	Optional<MammaOnderzoek> getLaatsteOnderzoekMetMissendeUitslagVanDossier(MammaDossier dossier);

	Specification<MammaOnderzoek> getLaatsteOnderzoekMetMissendeUitslagSpecification();
}
