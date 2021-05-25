package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleHuidigeRondePanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVisitatieBesprekenHuidigeRondePanel extends MammaKwaliteitscontroleHuidigeRondePanel
{
	private static final String ID_GEZIEN = "next";

	private final IModel<MammaVisitatieOnderzoek> visitatieOnderzoekModel;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	public MammaVisitatieBesprekenHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel, IModel<MammaVisitatieOnderzoek> visitatieOnderzoekModel)
	{
		super(id, beoordelingModel);
		this.visitatieOnderzoekModel = visitatieOnderzoekModel;
		setIngeklapt(true);
	}

	@Override
	protected void createButtons(WebMarkupContainer panelContainer, List<Component> buttons)
	{
		addButton(panelContainer, buttons, ID_GEZIEN, MammaVisitatieOnderzoekStatus.GEZIEN,
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN));
	}

	private void addButton(WebMarkupContainer panelContainer, List<Component> buttons, String id, MammaVisitatieOnderzoekStatus status, boolean btnVisible)
	{
		IndicatingAjaxLink<Void> button = new IndicatingAjaxLink<Void>(id)
		{
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
				volgende(target, status);
			}

		};

		MammaVisitatieOnderzoek visitatieOnderzoek = visitatieOnderzoekModel.getObject();
		button.setVisible(visitatieOnderzoek.getVisitatie().getAfgerondOp() == null && btnVisible);
		button.setOutputMarkupId(true);
		panelContainer.add(button);
		buttons.add(button);

	}

	private void volgende(AjaxRequestTarget target, MammaVisitatieOnderzoekStatus nieuweOnderzoekStatus)
	{
		kwaliteitscontroleService.wijzigOnderzoekStatus(visitatieOnderzoekModel.getObject(), nieuweOnderzoekStatus);
		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(visitatieOnderzoekModel);
	}
}
