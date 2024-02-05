package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.popup;

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

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaLogoutConfirmationDialog extends GenericPanel<Boolean>
{
	@SpringBean
	private MammaBeoordelingService beoordelingService;

	public MammaLogoutConfirmationDialog(String id, boolean heeftVerslagenTeBevestigen, boolean heeftOnderzoekenInWerklijst)
	{
		super(id);
		AjaxLink<Void> bevestigingLezingenBtn = new AjaxLink<Void>("bevestigenLezingen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				beoordelingService.bevestig1eEn2eLezingen(ScreenitSession.get().getLoggedInInstellingGebruiker());
				close(target, !heeftOnderzoekenInWerklijst);
			}
		};
		bevestigingLezingenBtn.setVisible(heeftVerslagenTeBevestigen);
		add(bevestigingLezingenBtn);
		add(new AjaxLink<Void>("uitloggen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target, true);
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				MammaLogoutConfirmationDialog.this.updateAjaxAttributes(attributes);
			}
		});

		WebMarkupContainer verslagOpenSection = new WebMarkupContainer("verslagenOpWerklijstMelding");
		verslagOpenSection.setVisible(heeftOnderzoekenInWerklijst);
		add(verslagOpenSection);
	}

	protected abstract void close(AjaxRequestTarget target, boolean logout);

	protected abstract void updateAjaxAttributes(AjaxRequestAttributes attributes);
}
