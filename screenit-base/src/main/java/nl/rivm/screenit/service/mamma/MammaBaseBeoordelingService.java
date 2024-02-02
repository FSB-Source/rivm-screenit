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
import java.util.function.Function;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;

public interface MammaBaseBeoordelingService
{
	boolean isBiradsVerwijzen(MammaBIRADSWaarde biradsWaarde);

	boolean isBiradsNietVerwijzen(MammaBIRADSWaarde biradsWaarde);

	boolean isLezingVerwijzen(MammaLezing lezing);

	void bevestigLezing(MammaBeoordeling beoordeling);

	void bevestigLezing(MammaBeoordeling beoordeling, boolean verstuurHl7Berichten);

	void discrepantieAfrondenEnNaarArbitrageZetten(MammaBeoordeling beoordeling, MammaLezing discrepantieLezing);

	void setStatusNaarVerslagGereed(MammaBeoordeling beoordeling);

	boolean isDiscrepantieVerwijzingVerplicht(MammaBeoordeling beoordeling, MammaZijde zijde);

	MammaBIRADSWaarde defaultBiradsWaarde(MammaBeoordeling modelObject, MammaZijde zijde);

	void slaLezingOpEnVerwerkStatus(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker loggedInInstellingGebruiker, StringResolver resolver);

	void verwerkBeoordelingStatusGunstigMetNevenbevindingen(MammaBeoordeling beoordeling);

	void slaLezingOp(MammaBeoordeling beoordeling, MammaLezing lezing);

	void wijsBeoordelingAanRadioloogToe(MammaBeoordeling beoordeling, InstellingGebruiker gebruiker);

	MammaLezing maakVerslagLezing(MammaBeoordeling beoordeling, MammaLezing uitgangsituatieLezing, InstellingGebruiker beoordelaar, boolean onervarenRadioloog);

	Boolean isUitslagGunstig(MammaBeoordeling beoordeling);

	MammaBIRADSWaarde getResultaatVoorZijde(MammaBeoordeling beoordeling, MammaZijde zijde);

	boolean iBiradsWaardeGeen(MammaLezing verslagLezing, MammaZijde zijde);

	MammaBeoordeling getBeoordelingVanLezing(MammaLezing lezing);

	Client getClientVanBeoordeling(MammaBeoordeling beoordeling);

	void setStatus(MammaBeoordeling beoordeling, MammaBeoordelingStatus status);

	MammaBIRADSWaarde getHoogsteBirads(MammaLezing lezing);

	MammaScreeningRonde getScreeningRonde(MammaBeoordeling beoordeling);

	MammaLezing getOrineleVerslagLezing(MammaBeoordeling beoordeling);

	String getMammaLezingEnumsTekst(Function<MammaLezing, List<? extends INaam>> getEnumListFromLezing, MammaLezing... lezingen);

	List<String> getNevenBevindingenOpmerkingenAsList(MammaBeoordeling beoordeling);

	boolean heeftBeoordelingNevenbevindingen(MammaBeoordeling beoordeling);

	String getNevenbevindingOpmerkingTekst(String lineBreak, MammaLezing... lezing);

	void verstuurXdsBericht(MammaBeoordeling beoordeling);

	void opgeschortOnderzoekTerugNaarWerklijst(MammaBeoordeling beoordeling);

	MammaBeoordeling getBeoordelingMetVerslagLezing(MammaAfspraak afspraak);

	MammaBeoordeling getBeoordelingMetEersteEnOfTweedeLezing(MammaAfspraak afspraak);

	Optional<MammaBeoordeling> zoekOpgeschorteBeoordelingInRonde(MammaScreeningRonde ronde, MammaBeoordelingOpschortenReden... opschortenRedenen);

	void annuleerBeoordeling(MammaBeoordeling beoordeling);

	void valideerEnHerbeoordeelBeoordeling(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker);

	boolean beoordelingZitInActieveFotobespreking(MammaBeoordeling beoordeling);

	MammaBeoordeling getLaatsteBeoordelingMetUitslag(MammaDossier dossier);
}
