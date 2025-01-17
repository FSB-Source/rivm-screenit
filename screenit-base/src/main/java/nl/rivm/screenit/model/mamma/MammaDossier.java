package nl.rivm.screenit.model.mamma;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(
	schema = "mamma",
	name = "dossier",
	indexes = {
		@Index(name = "idx_MAMMA_DOSSIER_STATUS", columnList = "status"),
		@Index(name = "idx_MAMMA_DOSSIER_FU_CONCLUSIE", columnList = "updateFollowUpConclusie"),
	},
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "laatste_screening_ronde"),
		@UniqueConstraint(columnNames = "laatste_afmelding"),
		@UniqueConstraint(columnNames = "screening_ronde_event"),
	})
@Audited
@Getter
@Setter
public class MammaDossier extends Dossier<MammaScreeningRonde, MammaAfmelding> implements DeelnamemodusDossier
{
	@OneToOne(mappedBy = "mammaDossier", optional = false, fetch = FetchType.LAZY)
	private Client client;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaScreeningRonde laatsteScreeningRonde;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaBeoordeling laatsteBeoordelingMetUitslag;

	@Column(nullable = false)
	private Boolean updateFollowUpConclusie = false;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfmelding laatsteAfmelding;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaDoelgroep doelgroep = MammaDoelgroep.REGULIER;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaTehuis tehuis;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date laatsteMammografieAfgerond;

	@Column(nullable = false)
	private Boolean eersteOnderzoek;

	@Transient
	@SkipFieldForDiff
	private Boolean uitTeNodigen;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	@SkipFieldForDiff
	private String dubbeleTijdReden;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	@SkipFieldForDiff
	private MammaKansberekeningScreeningRondeEvent screeningRondeEvent;

	@OneToOne(mappedBy = "dossier", optional = false, fetch = FetchType.LAZY)
	@NotAudited
	@SkipFieldForDiff
	private MammaDeelnamekans deelnamekans;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaIlmBezwaarPoging> ilmBezwaarPogingen = new ArrayList<>();

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private Deelnamemodus deelnamemodus = Deelnamemodus.STANDAARD;

	@OneToOne(optional = true, mappedBy = "dossier", fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@NotAudited
	private MammaVolgendeUitnodiging volgendeUitnodiging;

	@Transient
	public Boolean getUitTeNodigen()
	{
		return uitTeNodigen;
	}

	@Transient
	public void setUitTeNodigen(Boolean uitTeNodigen)
	{
		this.uitTeNodigen = uitTeNodigen;
	}
}
