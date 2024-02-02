
package nl.rivm.screenit.model.colon;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.ScannedFormulier;
import nl.rivm.screenit.model.UploadDocument;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Deprecated
@Entity
@Table(
	schema = "colon",
	indexes = @Index(name = "IDX_SCANANTWFORM_VERWERKT", columnList = "verwerkt"),
	uniqueConstraints = @UniqueConstraint(name = "uc_saf_objid", columnNames = "objid"))
@Audited
public class ScannedAntwoordFormulier extends ScannedFormulier
{

	public static final String STATUS_VERWIJDERD = "VERWIJDERD";

	public static final String STATUS_VERWIJDERD_UIT_DOSSIER = "VERWIJDERD_UIT_DOSSIER";

	public static final String STATUS_AFGEHANDELD = "AFGEHANDELD";

	private static final long serialVersionUID = 1L;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date orgineleScanDatum;

	@Temporal(TemporalType.DATE)
	private Date afnameDatum;

	@Column(name = "eenmaligAfmelden", nullable = false)
	private boolean eenmaligAfmelden;

	private int indexAfmeldReden;

	@Column(name = "definitiefAfmelden", nullable = false)
	private boolean definitiefAfmelden;

	private boolean handtekening;

	private String telefoonNummer1;

	private String telefoonNummer2;

	@Column(nullable = false)
	private boolean meedoenBVOenWO;

	@Column(nullable = false)
	private boolean toestemmingInzage;

	@Column(nullable = false)
	private boolean toestemmingBewaren;

	private boolean verwerkt;

	@ManyToOne
	private IFobtLaboratorium labId;

	private String status;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "scannedAntwoordFormulier")
	@NotAudited
	private List<SAFTransactionTrail> transactionTrails = new ArrayList<>();

	@OneToOne(optional = true)
	private UploadDocument formulier;

	public Date getOrgineleScanDatum()
	{
		return orgineleScanDatum;
	}

	public void setOrgineleScanDatum(Date orgineleScanDatum)
	{
		this.orgineleScanDatum = orgineleScanDatum;
	}

	public Date getAfnameDatum()
	{
		return afnameDatum;
	}

	public void setAfnameDatum(Date afnameDatum)
	{
		this.afnameDatum = afnameDatum;
	}

	public boolean isEenmaligAfmelden()
	{
		return eenmaligAfmelden;
	}

	public void setEenmaligAfmelden(boolean eenmaligAfmelden)
	{
		this.eenmaligAfmelden = eenmaligAfmelden;
	}

	public int getIndexAfmeldReden()
	{
		return indexAfmeldReden;
	}

	public void setIndexAfmeldReden(int indexAfmeldReden)
	{
		this.indexAfmeldReden = indexAfmeldReden;
	}

	public boolean isDefinitiefAfmelden()
	{
		return definitiefAfmelden;
	}

	public void setDefinitiefAfmelden(boolean definitiefAfmelden)
	{
		this.definitiefAfmelden = definitiefAfmelden;
	}

	public boolean isHandtekening()
	{
		return handtekening;
	}

	public void setHandtekening(boolean handtekening)
	{
		this.handtekening = handtekening;
	}

	public String getTelefoonNummer1()
	{
		return telefoonNummer1;
	}

	public void setTelefoonNummer1(String telefoonNummer1)
	{
		this.telefoonNummer1 = telefoonNummer1;
	}

	public String getTelefoonNummer2()
	{
		return telefoonNummer2;
	}

	public void setTelefoonNummer2(String telefoonNummer2)
	{
		this.telefoonNummer2 = telefoonNummer2;
	}

	public boolean isMeedoenBVOenWO()
	{
		return meedoenBVOenWO;
	}

	public void setMeedoenBVOenWO(boolean meedoenBVOenWO)
	{
		this.meedoenBVOenWO = meedoenBVOenWO;
	}

	public boolean isToestemmingInzage()
	{
		return toestemmingInzage;
	}

	public void setToestemmingInzage(boolean toestemmingInzage)
	{
		this.toestemmingInzage = toestemmingInzage;
	}

	public boolean isToestemmingBewaren()
	{
		return toestemmingBewaren;
	}

	public void setToestemmingBewaren(boolean toestemmingBewaren)
	{
		this.toestemmingBewaren = toestemmingBewaren;
	}

	public boolean isVerwerkt()
	{
		return verwerkt;
	}

	public void setVerwerkt(boolean verwerkt)
	{
		this.verwerkt = verwerkt;
	}

	public IFobtLaboratorium getLabId()
	{
		return labId;
	}

	public void setLabId(IFobtLaboratorium labId)
	{
		this.labId = labId;
	}

	public List<SAFTransactionTrail> getTransactionTrails()
	{
		return transactionTrails;
	}

	public void setTransactionTrails(List<SAFTransactionTrail> transactionTrails)
	{
		this.transactionTrails = transactionTrails;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public UploadDocument getFormulier()
	{
		return formulier;
	}

	public void setFormulier(UploadDocument formulier)
	{
		this.formulier = formulier;
	}
}
