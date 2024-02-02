package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.main.model.GeenHuisartsModel;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.huisarts.HuisartsInfoPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaHuisartsBeheerPanel extends GenericPanel<MammaScreeningRonde>
{

	@SpringBean
	private RondeNummerService rondeNummerService;

	private final boolean voorPrimaireHuisarts;

	public MammaHuisartsBeheerPanel(String id, IModel<MammaScreeningRonde> screeningRondeModel, BootstrapDialog dialog)
	{
		this(id, screeningRondeModel, dialog, true);
	}

	public MammaHuisartsBeheerPanel(String id, IModel<MammaScreeningRonde> screeningRondeModel, BootstrapDialog dialog, boolean voorPrimaireHuisarts)
	{
		super(id, screeningRondeModel);
		this.voorPrimaireHuisarts = voorPrimaireHuisarts;

		boolean magWijzigen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_WIJZIGEN_HUISARTS, Actie.AANPASSEN);

		AjaxLink<MammaScreeningRonde> wijzigHuisartsBtn = new IndicatingAjaxLink<MammaScreeningRonde>("wijzigHuisarts")
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(magWijzigen);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				EnovationHuisarts huisartsVorigeRonde = getHuisartsVorigeRonde();
				if (huisartsVorigeRonde != null)
				{
					openHuisartsUitVorigeRondePopup(dialog, target, huisartsVorigeRonde);
				}
				else
				{
					openZoekHuisartsPopup(dialog, target, false);
				}
			}

		};
		wijzigHuisartsBtn.setVisible(magWijzigen);
		add(wijzigHuisartsBtn);

		EnovationHuisarts enovationHuisarts = getModelObject().getHuisarts();

		AjaxLink<Void> huisartsVerwijderenBtn = new IndicatingAjaxLink<Void>("verwijderHuisarts")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaHuisartsBeheerPanel.this.onHuisartsGekozen(target, null, null);
			}
		};
		add(huisartsVerwijderenBtn);
		huisartsVerwijderenBtn.setVisible(!voorPrimaireHuisarts);

		MammaGeenHuisartsOption geenHuisartsOptie = getModelObject().getGeenHuisartsOptie();
		BaseHuisartsModel<?> huisartsModel = null;
		if (enovationHuisarts != null)
		{
			huisartsModel = new EnovationHuisartsModel(enovationHuisarts);
		}
		else
		{
			huisartsModel = new GeenHuisartsModel();
		}
		add(new HuisartsInfoPanel("huisartsInfo", huisartsModel).setVisible(geenHuisartsOptie == null));

		add(new EnumLabel<MammaGeenHuisartsOption>("geenHuisartsOptie", geenHuisartsOptie).setVisible(geenHuisartsOptie != null));
	}

	private void openHuisartsUitVorigeRondePopup(BootstrapDialog dialog, AjaxRequestTarget target, EnovationHuisarts huisartsVorigeRonde)
	{
		dialog.openWith(target, new MammaHuisartsVorigeRondePopupPanel(IDialog.CONTENT_ID, ModelUtil.cRModel(huisartsVorigeRonde))
		{
			@Override
			protected void close(AjaxRequestTarget target)
			{
				dialog.close(target);
			}

			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
			{
				MammaHuisartsBeheerPanel.this.onHuisartsGekozen(target, huisarts, geenHuisartsOptie);
			}

			@Override
			protected void openZoekHuisartsPopup(AjaxRequestTarget target)
			{
				MammaHuisartsBeheerPanel.this.openZoekHuisartsPopup(dialog, target, true);
			}

		});
	}

	private void openZoekHuisartsPopup(BootstrapDialog dialog, AjaxRequestTarget target, boolean terugNaarZoeken)
	{
		dialog.openWith(target, new MammaHuisartsZoekenPopupPanel(IDialog.CONTENT_ID, terugNaarZoeken, voorPrimaireHuisarts)
		{

			@Override
			protected void close(AjaxRequestTarget target)
			{
				dialog.close(target);
			}

			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
			{
				MammaHuisartsBeheerPanel.this.onHuisartsGekozen(target, huisarts, geenHuisartsOptie);
			}
		});
	}

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie);

	protected abstract EnovationHuisarts getHuisartsVorigeRonde();
}
