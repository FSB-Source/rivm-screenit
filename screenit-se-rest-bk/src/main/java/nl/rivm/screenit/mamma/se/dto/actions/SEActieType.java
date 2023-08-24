package nl.rivm.screenit.mamma.se.dto.actions;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.enums.Recht;

public enum SEActieType
{
	INSCHRIJVEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	CLIENTGEGEVENS_OPSLAAN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	AFSPRAAK_MAKEN_PASSANT(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	UITSCHRIJVEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	SET_EMAILADRES(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	ONDERZOEK_STARTEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	MAMMOGRAFIE_OPSLAAN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	ONDERZOEK_OPSLAAN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	AFSPRAAK_SIGNALEREN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	AFSPRAAK_AFRONDEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	AFSPRAAK_SIGNALEREN_OPSLAAN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	ONDERZOEK_AFRONDEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	LOG_GEBEURTENIS_GEFAALDE_TRANSACTIE(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	MAAK_DUBBELE_TIJD(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	MAAK_DUBBELE_TIJD_REDEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	UPDATE_HEEFT_AFWIJKINGEN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	AFSPRAAK_DOORVOEREN(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK),
	MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	SET_SIGNALERING(Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN),
	SET_VISUELE_INSPECTIE_AFBEELDING(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	WIJZIGINGEN_GEMAAKT(Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN),
	START_KWALITEITSOPNAME(Recht.GEBRUIKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME),
	BEEINDIG_KWALITEITSOPNAME(Recht.GEBRUIKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME);

	private Recht recht;

	SEActieType(Recht recht)
	{
		this.recht = recht;
	}

	public Recht getRecht()
	{
		return recht;
	}
}
