package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixBarcodeAfdrukkenBasePage;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixMonsterSignalering;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class CervixUitnodigingUitstrijkjePanel extends CervixUitnodigingPanel<CervixUitstrijkje>
{
	@SpringBean
	private CervixBaseUitnodigingService uitnodingingService;

	private ScreenitDropdown<CervixUitstrijkjeStatus> uitstrijkjeStatusDropdown = null;

	private Date verzenddatumUitnodiging;

	public CervixUitnodigingUitstrijkjePanel(CervixBarcodeAfdrukkenBasePage parentPage, String id, CervixUitstrijkje uitstrijkje)
	{
		super(parentPage, id, uitstrijkje);
		MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(uitstrijkje.getUitnodiging().getBrief());
		if (mergedBrieven != null)
		{
			verzenddatumUitnodiging = mergedBrieven.getPrintDatum();
		}
	}

	@Override
	protected String getStatus()
	{
		return getString(EnumStringUtil.getPropertyString(getModelObject().getUitstrijkjeStatus()));
	}

	@Override
	protected boolean reedsIngeboekt()
	{
		return getModelObject().getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN;
	}

	@Override
	protected boolean nuInboeken()
	{
		return !reedsIngeboekt() && ontvangstMonster() && verzenddatumUitnodiging != null;
	}

	@Override
	protected void inboeken()
	{
		CervixUitstrijkje uitstrijkje = getModelObject();
		uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.ONTVANGEN);
		saveMonster(null);
	}

	@Override
	protected void addMonsterTypeSpecifics(ScreenitForm<CervixUitstrijkje> form, WebMarkupContainer labformulierLaboratoriumContainer, BMHKLaboratorium laboratorium,
		boolean ingeboektInAnderLaboratorium)
	{
		CervixUitstrijkje uitstrijkje = getModelObject();
		CervixUitstrijkjeStatus uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();

		form.add(new Label("verzenddatum", verzenddatumUitnodiging == null ? "" : verzenddatumUitnodiging));
		form.add(DateLabel.forDatePattern("labformulier.datumUitstrijkje", Constants.DEFAULT_DATE_FORMAT));

		List<CervixUitstrijkjeStatus> mogelijkeUitstrijkjeStatussen = getMogelijkeUitstrijkjeStatussen(uitstrijkjeStatus);
		uitstrijkjeStatusDropdown = new ScreenitDropdown<>("monsterStatus", new PropertyModel<>(getModel(), "uitstrijkjeStatus"), mogelijkeUitstrijkjeStatussen,
			new EnumChoiceRenderer<>());
		form.add(uitstrijkjeStatusDropdown);

		boolean enabled = ontvangstMonster()
			&& !ingeboektInAnderLaboratorium && (uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ONTVANGEN)
				|| uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.ONTVANGEN) || uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR))
			&& verzenddatumUitnodiging != null && uitstrijkje.getBrief() == null;
		uitstrijkjeStatusDropdown.setEnabled(enabled);
		nietAnalyseerbaarReden.setEnabled(enabled);
		monsterSignaleringen.setEnabled(enabled);
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		if (labformulier != null && labformulier.getDigitaal())
		{
			for (CervixMonsterSignalering signalering : monsterSignaleringen.getChoices())
			{
				if (signalering.equals(CervixMonsterSignalering.GEEN_AANVRAAG_FORMULIER_WEL_MONSTER)
					|| signalering.equals(CervixMonsterSignalering.KOPIE_AANVRAAGFORMULIER_GEBRUIKT))
				{
					monsterSignaleringen.remove(String.valueOf(signalering));
				}
			}
		}
		overigeSignalering.setEnabled(enabled);

		form.add(new EnumLabel<CervixLabformulierStatus>("labformulier.status"));
		form.add(new BooleanLabel("labformulier.digitaal"));

		boolean labformulierInAnderLaboratorium = false;
		if (labformulier != null)
		{
			labformulierInAnderLaboratorium = !laboratorium.equals(labformulier.getLaboratorium());
		}
		labformulierLaboratoriumContainer.setVisible(labformulierInAnderLaboratorium);
		labformulierLaboratoriumContainer.add(new Label("labformulier.laboratorium.naam"));

		nietAnalyseerbaarReden.setVisible(getModelObject().getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR);
		monsterSignaleringenContainer.setVisible(getModelObject().getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN);
	}

	@Override
	protected void setStatusDropdownChoices(AjaxRequestTarget target)
	{
		uitstrijkjeStatusDropdown.setChoices(getMogelijkeUitstrijkjeStatussen(getModelObject().getUitstrijkjeStatus()));
		target.add(uitstrijkjeStatusDropdown);
	}

	private List<CervixUitstrijkjeStatus> getMogelijkeUitstrijkjeStatussen(CervixUitstrijkjeStatus huidigeUitstrijkjeStatus)
	{
		List<CervixUitstrijkjeStatus> mogelijkeUitstrijkjeStatussen = new ArrayList<>();
		mogelijkeUitstrijkjeStatussen.add(CervixUitstrijkjeStatus.ONTVANGEN);

		if (getModelObject().getCytologieOrder() == null)
		{

			CervixScreeningRonde ontvangstRonde = getModelObject().getOntvangstScreeningRonde();
			if (ontvangstRonde != null
				&& (ontvangstRonde.getMonsterHpvUitslag() == null
				|| ontvangstRonde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag().equals(CervixHpvBeoordelingWaarde.POSITIEF)
				&& ontvangstRonde.getUitstrijkjeCytologieUitslag() == null
				|| ontvangstRonde.getInVervolgonderzoekDatum() != null && ontvangstRonde.getUitstrijkjeVervolgonderzoekUitslag() == null))
			{
				mogelijkeUitstrijkjeStatussen.add(CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR);
			}

			if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_ONTVANGST_MONSTER, Actie.VERWIJDEREN))
			{
				mogelijkeUitstrijkjeStatussen.add(CervixUitstrijkjeStatus.NIET_ONTVANGEN);
			}
		}

		if (!mogelijkeUitstrijkjeStatussen.contains(huidigeUitstrijkjeStatus))
		{
			mogelijkeUitstrijkjeStatussen.add(huidigeUitstrijkjeStatus);
		}
		return mogelijkeUitstrijkjeStatussen;
	}

	@Override
	protected void saveMonster(AjaxRequestTarget target)
	{
		boolean success = true;

		CervixUitstrijkje uitstrijkje = getModelObject();
		CervixUitstrijkjeStatus status = uitstrijkje.getUitstrijkjeStatus();

		monsterSignaleringenContainer.setVisible(status != CervixUitstrijkjeStatus.NIET_ONTVANGEN);

		if (status != CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR)
		{
			uitstrijkje.setNietAnalyseerbaarReden(null);
			nietAnalyseerbaarReden.setVisible(false);
		}
		else
		{
			if (uitstrijkje.getNietAnalyseerbaarReden() == null)
			{
				nietAnalyseerbaarReden.setVisible(true);
				uitstrijkje.setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden.ONBEKEND);
			}

			if (uitstrijkje.getNietAnalyseerbaarReden() == CervixNietAnalyseerbaarReden.ONBEKEND)
			{
				success = false;
				error(getString("reden.verplicht"));
			}
		}

		if (target != null)
		{
			target.add(nietAnalyseerbaarReden);
		}
		String logMessage = getString("titel") + " - " + getStatus();
		if (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR)
		{
			logMessage = logMessage + " (reden: " + uitstrijkje.getNietAnalyseerbaarReden().getNaam() + ")";
		}

		logMessage = logMessage + getSignaleringen();

		uitnodingingService.saveMonster(uitstrijkje, ScreenitSession.get().getLoggedInInstellingGebruiker(), logMessage);
		if (success)
		{
			success(getString("uitnodiging.opgeslagen"));
		}
	}

	@Override
	protected void registreerBarcodeAfgedrukt(AjaxRequestTarget target)
	{
		uitnodingingService.registreerMonsterBarcodeAfgedrukt(getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker(),
			LogGebeurtenis.CERVIX_UITSTRIJKJE_BARCODE_AFGEDRUKT);
	}
}
