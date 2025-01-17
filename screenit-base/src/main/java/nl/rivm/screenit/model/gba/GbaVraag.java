package nl.rivm.screenit.model.gba;

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

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Check;

@Entity
@Table(schema = "algemeen", indexes = @Index(name = "idx_gba_vraag_bsn", columnList = "bsn"))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Getter
@Setter
@Check(constraints = "(bsn is not null or client is not null) AND"
	+ "("
	+ "  reden IN ('ONJUIST_ADRES', 'ONJUISTE_PERSOONSGEGEVENS', 'MUTATIEBERICHT_ONBEKENDE_CLIENT')"
	+ "  OR (vraag_type = 'VERWIJDER_INDICATIE' AND reden in ('BEZWAAR', 'AFGEMELD', 'BOVENGRENS_LEEFTIJD', 'SELECTIEBLOKKADE'))"
	+ "  OR (vraag_type = 'PLAATS_INDICATIE' AND reden in ('BEZWAAR_INGETROKKEN', 'AANGEMELD', 'BRIEF_VERSTUREN', 'ONVERWACHT_INDICATIE_VERWIJDERD'))"
	+ ")"
)
public class GbaVraag extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	private Client client;

	private String bsn;

	@Column(nullable = false)
	private LocalDateTime datum;

	private boolean verstuurd;

	private String uniqueBatchId;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private GbaVraagType vraagType;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private RedenGbaVraag reden;

	private boolean reactieOntvangen;

	private String aanvullendeInformatie;
}
