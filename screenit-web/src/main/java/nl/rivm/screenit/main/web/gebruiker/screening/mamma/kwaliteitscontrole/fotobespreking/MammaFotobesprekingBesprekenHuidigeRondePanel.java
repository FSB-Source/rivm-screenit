package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleHuidigeRondePanel;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaFotobesprekingBesprekenHuidigeRondePanel extends MammaKwaliteitscontroleHuidigeRondePanel
{
	private static final String ID_OPNIEUW_BEOORDELEN = "opnieuwBeoordelen";

	private static final String ID_BESPROKEN = "next";

	private final IModel<MammaFotobesprekingOnderzoek> fotobesprekingOnderzoekModel;

	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	public MammaFotobesprekingBesprekenHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel, IModel<MammaFotobesprekingOnderzoek> fotobesprekingOnderzoekModel)
	{
		super(id, beoordelingModel);
		this.fotobesprekingOnderzoekModel = fotobesprekingOnderzoekModel;
		setIngeklapt(false);
	}

	@Override
	protected void createButtons(WebMarkupContainer panelContainer, List<Component> buttons)
	{
		addButton(panelContainer, buttons, ID_BESPROKEN, MammaFotobesprekingOnderzoekStatus.BESPROKEN);
		addButton(panelContainer, buttons, ID_OPNIEUW_BEOORDELEN, MammaFotobesprekingOnderzoekStatus.NIEUWE_BEOORDELING_AANGEMAAKT);
	}

	private void addButton(WebMarkupContainer panelContainer, List<Component> buttons, final String id, MammaFotobesprekingOnderzoekStatus status)
	{
		ConfirmingIndicatingAjaxLink<Void> button = new ConfirmingIndicatingAjaxLink<Void>(id, ((GebruikerBasePage) getPage()).getDialog(), "confirm.herbeoordelen")
		{
			private boolean bevestigingNietNodig = !ID_OPNIEUW_BEOORDELEN.equals(id);

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				AjaxCallListener myAjaxCallListener = new AjaxCallListener();
				myAjaxCallListener.onBefore("logOnAfrondenClick();");
				attributes.getAjaxCallListeners().add(myAjaxCallListener);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				LogGebeurtenis logGebeurtenis = ID_BESPROKEN.equals(id) ? LogGebeurtenis.FOTOBESPREKING_BESPROKEN
					: ID_OPNIEUW_BEOORDELEN.equals(id) ? LogGebeurtenis.FOTOBESPREKING_OPNIEUW_BEOORDELEN
					: null;
				if (logGebeurtenis != null)
				{
					logService.logGebeurtenis(
						logGebeurtenis,
						ScreenitSession.get().getLoggedInAccount(),
						fotobesprekingOnderzoekModel.getObject().getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient());
				}
				volgende(target, status);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return bevestigingNietNodig;
			}

		};

		MammaFotobesprekingOnderzoek fotobesprekingOnderzoek = fotobesprekingOnderzoekModel.getObject();
		if (fotobesprekingOnderzoek.getFotobespreking().getAfgerondOp() == null)
		{
			boolean heeftImsDesktopSyncRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN);
			switch (fotobesprekingOnderzoek.getStatus())
			{
			case NIET_BESPROKEN:
			case BESPROKEN:
				button.setVisible(heeftImsDesktopSyncRecht
					&& (ID_BESPROKEN.equals(id) || (!MammaBeoordelingStatus.UITSLAG_ONGUNSTIG.equals(fotobesprekingOnderzoek.getBeoordeling().getStatus())))
					&& (ID_BESPROKEN.equals(id)
					|| (ID_OPNIEUW_BEOORDELEN.equals(id) && !MammaBeoordelingStatus.GEANNULEERD.equals(fotobesprekingOnderzoek.getBeoordeling().getStatus()))));
				break;
			default:
				button.setVisible(false);
				break;
			}
		}
		else
		{
			button.setVisible(false);
		}
		button.setOutputMarkupId(true);
		panelContainer.add(button);
		buttons.add(button);

	}

	private void volgende(AjaxRequestTarget target, MammaFotobesprekingOnderzoekStatus nieuweOnderzoekStatus)
	{
		if (nieuweOnderzoekStatus == MammaFotobesprekingOnderzoekStatus.NIEUWE_BEOORDELING_AANGEMAAKT)
		{
			try
			{
				kwaliteitscontroleService.herbeoordeelFotobesprekingOnderzoek(fotobesprekingOnderzoekModel.getObject());
				((AbstractMammaBeoordelenPage) getPage()).volgendeBeoordeling(target);
			}
			catch (Exception e)
			{
				GbaPersoon persoon = getModelObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon();
				warn(String.format(getString("error.herbeoordelen"),
					persoon.getBsn(),
					DateUtil.getGeboortedatum(persoon),
					e instanceof IllegalStateException ? e.getMessage() : "onbekende fout"));
			}
		}
		else
		{
			kwaliteitscontroleService.wijzigOnderzoekStatus(fotobesprekingOnderzoekModel.getObject(), nieuweOnderzoekStatus);
			((AbstractMammaBeoordelenPage) getPage()).volgendeBeoordeling(target);
		}
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(fotobesprekingOnderzoekModel);
	}
}
