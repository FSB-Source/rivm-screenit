package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixBarcodeAfdrukkenBasePage;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.rivm.screenit.util.EnumStringUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CervixUitnodigingZasPanel extends CervixUitnodigingPanel<CervixZas>
{

	@SpringBean
	private CervixBaseUitnodigingService uitnodingingService;

	@SpringBean
	private BaseHoudbaarheidService houdbaarheidService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private CervixDossierDao dossierDao;

	private ScreenitDropdown<CervixZasStatus> zasStatusDropdown = null;

	public CervixUitnodigingZasPanel(CervixBarcodeAfdrukkenBasePage parentPage, String id, CervixZas zas)
	{
		super(parentPage, id, zas);
	}

	@Override
	protected String getStatus()
	{
		return getString(EnumStringUtil.getPropertyString(getModelObject().getZasStatus()));
	}

	@Override
	protected boolean reedsIngeboekt()
	{
		return getModelObject().getZasStatus() != CervixZasStatus.VERSTUURD;
	}

	@Override
	protected boolean nuInboeken()
	{
		return !reedsIngeboekt() && ontvangstMonster();
	}

	@Override
	protected void inboeken()
	{
		CervixZas zas = getModelObject();
		zas.setZasStatus(CervixZasStatus.ONTVANGEN);
		if (!isHoudbaar(zas))
		{
			zas.setZasStatus(CervixZasStatus.NIET_ANALYSEERBAAR);
			zas.setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden.HOUDBAARHEID_VERLOPEN_OF_ONBEKEND);
		}
		saveMonster(null);
	}

	private boolean isHoudbaar(CervixZas zas)
	{
		return houdbaarheidService.isHoudbaar(CervixZasHoudbaarheid.class, zas.getMonsterId());
	}

	@Override
	protected void addMonsterTypeSpecifics(ScreenitForm<CervixZas> form, WebMarkupContainer labformulierLaboratoriumContainer, BMHKLaboratorium laboratorium,
		boolean ingeboektInAnderLaboratorium)
	{
		CervixZas zas = getModelObject();
		CervixZasStatus zasStatus = zas.getZasStatus();

		form.add(new Label("verzenddatum", zas.getVerstuurd()));

		List<CervixZasStatus> mogelijkeZasStatussen = getMogelijkeZasStatussen(zasStatus);
		zasStatusDropdown = new ScreenitDropdown<>("monsterStatus", new PropertyModel<>(getModel(), "zasStatus"), mogelijkeZasStatussen, new EnumChoiceRenderer<>());
		form.add(zasStatusDropdown);

		boolean enabled = ontvangstMonster() && !ingeboektInAnderLaboratorium
			&& (zasStatus.equals(CervixZasStatus.VERSTUURD) || zasStatus.equals(CervixZasStatus.ONTVANGEN) || zasStatus.equals(CervixZasStatus.NIET_ANALYSEERBAAR))
			&& zas.getBrief() == null;
		zasStatusDropdown.setEnabled(enabled);
		nietAnalyseerbaarReden.setEnabled(enabled);
		monsterSignaleringen.setEnabled(enabled);
		overigeSignalering.setEnabled(enabled);

		labformulierLaboratoriumContainer.setVisible(false);
		form.add(new Label("labformulier.status", ""));
		form.add(new Label("labformulier.digitaal", ""));
		labformulierLaboratoriumContainer.add(new Label("labformulier.laboratorium.naam", ""));
		Label datumUitstrijkje = new Label("labformulier.datumUitstrijkje", "");
		datumUitstrijkje.setVisible(false);
		form.add(datumUitstrijkje);

		nietAnalyseerbaarReden.setVisible(
			zas.getZasStatus() == CervixZasStatus.NIET_ANALYSEERBAAR && zas.getNietAnalyseerbaarReden() != CervixNietAnalyseerbaarReden.HOUDBAARHEID_VERLOPEN_OF_ONBEKEND);
		monsterSignaleringenContainer.setVisible(zas.getZasStatus() != CervixZasStatus.VERSTUURD);
	}

	@Override
	protected void setStatusDropdownChoices(AjaxRequestTarget target)
	{
		zasStatusDropdown.setChoices(getMogelijkeZasStatussen(getModelObject().getZasStatus()));
		target.add(zasStatusDropdown);
	}

	private List<CervixZasStatus> getMogelijkeZasStatussen(CervixZasStatus huidigeZasStatus)
	{
		List<CervixZasStatus> mogelijkeZasStatussen = new ArrayList<>();
		CervixZas zas = getModelObject();
		if (isHoudbaar(zas))
		{
			mogelijkeZasStatussen.add(CervixZasStatus.ONTVANGEN);
		}

		CervixScreeningRonde ontvangstRonde = zas.getOntvangstScreeningRonde();
		if (ontvangstRonde != null && ontvangstRonde.getMonsterHpvUitslag() == null)
		{
			mogelijkeZasStatussen.add(CervixZasStatus.NIET_ANALYSEERBAAR);
		}

		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_ONTVANGST_MONSTER, Actie.VERWIJDEREN))
		{
			mogelijkeZasStatussen.add(CervixZasStatus.VERSTUURD);
		}
		if (!mogelijkeZasStatussen.contains(huidigeZasStatus))
		{
			mogelijkeZasStatussen.add(huidigeZasStatus);
		}
		return mogelijkeZasStatussen;
	}

	@Override
	protected void saveMonster(AjaxRequestTarget target)
	{
		boolean success = true;

		CervixZas zas = getModelObject();
		CervixZasStatus status = zas.getZasStatus();

		monsterSignaleringenContainer.setVisible(status != CervixZasStatus.VERSTUURD);

		if (status != CervixZasStatus.NIET_ANALYSEERBAAR)
		{
			zas.setNietAnalyseerbaarReden(null);
			nietAnalyseerbaarReden.setVisible(false);
		}
		else
		{
			if (zas.getNietAnalyseerbaarReden() != CervixNietAnalyseerbaarReden.HOUDBAARHEID_VERLOPEN_OF_ONBEKEND)
			{
				nietAnalyseerbaarReden.setVisible(true);
			}

			if (zas.getNietAnalyseerbaarReden() == null)
			{
				zas.setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden.ONBEKEND);
			}

			if (zas.getNietAnalyseerbaarReden() == CervixNietAnalyseerbaarReden.ONBEKEND)
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
		if (CervixZasStatus.NIET_ANALYSEERBAAR.equals(zas.getZasStatus()))
		{
			logMessage = logMessage + " (reden: " + zas.getNietAnalyseerbaarReden().getNaam() + ")";
		}

		logMessage = logMessage + getSignaleringen();

		uitnodingingService.saveMonster(zas, ScreenitSession.get().getLoggedInInstellingGebruiker(), logMessage);

		String feedback = getString("uitnodiging.opgeslagen");
		boolean hasFeedbackMessage = getFeedbackMessages().hasMessage(new IFeedbackMessageFilter()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public boolean accept(FeedbackMessage message)
			{
				return message.getMessage().equals(feedback);
			}

		});
		if (!hasFeedbackMessage && success)
		{
			success(feedback);
		}
	}

	@Override
	protected void registreerBarcodeAfgedrukt(AjaxRequestTarget target)
	{
		uitnodingingService.registreerMonsterBarcodeAfgedrukt(getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker(),
			LogGebeurtenis.CERVIX_ZAS_BARCODE_AFGEDRUKT);
	}
}
